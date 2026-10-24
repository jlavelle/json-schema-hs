module Text.Regex.Ecma262.Internal.Dfa
  ( Dfa
  , Answer (..)
  , build
  , matchDfa
  ) where

import Control.Monad qualified as Monad
import Control.Monad.ST qualified as ST
import Control.Monad.Trans.Class qualified as Trans
import Control.Monad.Trans.State.Strict qualified as State
import Data.Bits qualified as Bits
import Data.Foldable qualified as Foldable
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Primitive.Array qualified as Array
import Data.Primitive.MutVar qualified as MutVar
import Data.Primitive.PrimArray qualified as PrimArray
import Data.Word qualified as Word

import Text.Regex.Ecma262.Internal.Charset qualified as Charset
import Text.Regex.Ecma262.Internal.Parse qualified as Parse
import Text.Regex.Ecma262.Internal.Utf8 qualified as Utf8

data Dfa = Dfa
  { dfaByteClass :: !(PrimArray.PrimArray Word.Word8)
  , dfaTrans :: !(PrimArray.PrimArray Word.Word8)
  , dfaShift :: !Int
  , dfaStart :: !Int
  , dfaAtEnd :: !(PrimArray.PrimArray Word.Word8)
  , dfaEmpty :: !Bool
  }

data Answer
  = Matched
  | NoMatch
  | Undecided
  deriving (Eq, Show)

build :: Parse.Node [Int] -> Maybe Dfa
build root = do
  (start, nfaBuilder) <-
    State.runStateT
      (addState NfaMatch *> compileNode root matchState)
      NfaBuilder{nfaStates = IntMap.empty, nfaCount = 0}
  let
    nfa = Array.arrayFromList (IntMap.elems (nfaStates nfaBuilder))
    nfaSize = Array.sizeofArray nfa
    sets =
      foldMap
        ( \k -> case Array.indexArray nfa k of
            NfaSet bits out -> [(k, bits, out)]
            _ -> []
        )
        [0 .. nfaSize - 1]
    refine parts mask =
      foldMap
        ( \part ->
            filter
              (/= Charset.asciiNone)
              [Charset.asciiIntersection part mask, Charset.asciiIntersection part (Charset.asciiComplement mask)]
        )
        parts
    partition = Foldable.foldl' refine [Charset.asciiAll] (fmap (\(_, bits, _) -> bits) sets)
    firstMember part = length (takeWhile (not . Charset.asciiHas part) [0 .. 127 :: Int])
    numbered = zip (List.sortOn firstMember partition) [1 :: Int ..]
    classOf c = maybe 0 snd (List.find (\(part, _) -> Charset.asciiHas part c) numbered)
    byteClassOf = fmap classOf [0 .. 127]
    classes = length numbered + 1
    shift = length (takeWhile (< classes) (iterate (* 2) 1))
    stride = Bits.shiftL 1 shift :: Int
    sample = IntMap.fromList (reverse (zip byteClassOf [0 :: Int ..]))
    movesOf =
      IntMap.map (\c -> fmap (\(k, _, out) -> (k, out)) (filter (\(_, bits, _) -> Charset.asciiHas bits c) sets)) sample
  ST.runST $ do
    mark <- PrimArray.newPrimArray nfaSize
    PrimArray.setPrimArray mark 0 nfaSize 0
    gen <- MutVar.newMutVar 0
    steps <- MutVar.newMutVar maxDfaSteps
    ids <- MutVar.newMutVar Map.empty
    setsById <- MutVar.newMutVar IntMap.empty
    atEnds <- MutVar.newMutVar IntMap.empty
    count <- MutVar.newMutVar dfaFirst
    table <- PrimArray.newPrimArray (maxDfaStates * stride)
    PrimArray.setPrimArray table 0 (maxDfaStates * stride) (fromIntegral dfaDead)
    PrimArray.setPrimArray table (dfaAccept * stride) stride (fromIntegral dfaAccept)
    PrimArray.setPrimArray table (dfaBail * stride) stride (fromIntegral dfaBail)
    let
      b = Builder{builderNfa = nfa, builderMark = mark, builderGen = gen, builderSteps = steps}
      intern set
        | hasMatch set = pure (Just dfaAccept)
        | IntSet.null set = pure (Just dfaDead)
        | otherwise = do
            known <- fmap (Map.lookup set) (MutVar.readMutVar ids)
            case known of
              Just found -> pure (Just found)
              Nothing -> do
                k <- MutVar.readMutVar count
                if k >= maxDfaStates
                  then pure Nothing
                  else do
                    MutVar.modifyMutVar' ids (Map.insert set k)
                    MutVar.modifyMutVar' setsById (IntMap.insert k set)
                    MutVar.writeMutVar count (k + 1)
                    let
                      ends =
                        foldMap
                          ( \j -> case Array.indexArray nfa j of
                              NfaEol out -> [out]
                              _ -> []
                          )
                          (IntSet.toList set)
                    endsClosed <- if null ends then pure IntSet.empty else closure b False True ends
                    MutVar.modifyMutVar' atEnds (IntMap.insert k (not (null ends) && hasMatch endsClosed))
                    pure (Just k)
      fillRow restart set k cls
        | cls >= stride = pure True
        | cls == 0 || cls >= classes = do
            PrimArray.writePrimArray table (k * stride + cls) (fromIntegral dfaBail)
            fillRow restart set k (cls + 1)
        | otherwise = do
            let
              moved = fmap snd . filter ((`IntSet.member` set) . fst) $ IntMap.findWithDefault [] cls movesOf
            next <- closure b False False (moved <> restart)
            target <- intern next
            left <- MutVar.readMutVar steps
            case target of
              Just t
                | left >= 0 -> do
                    PrimArray.writePrimArray table (k * stride + cls) (fromIntegral t)
                    fillRow restart set k (cls + 1)
              _ -> pure False
      fillRows restart k = do
        n <- MutVar.readMutVar count
        if k >= n
          then pure True
          else do
            set <- fmap (IntMap.findWithDefault IntSet.empty k) (MutVar.readMutVar setsById)
            filled <- fillRow restart set k 0
            if filled then fillRows restart (k + 1) else pure False
    restart <- fmap IntSet.toList (closure b False False [start])
    emptyMatch <- fmap hasMatch (closure b True True [start])
    first <- closure b True False [start]
    startId <- intern first
    built <- fillRows restart dfaFirst
    n <- MutVar.readMutVar count
    ends <- MutVar.readMutVar atEnds
    frozen <- PrimArray.freezePrimArray table 0 (n * stride)
    pure $ case startId of
      Just s
        | built ->
            Just
              Dfa
                { dfaByteClass = PrimArray.primArrayFromList (fmap fromIntegral (byteClassOf <> replicate 128 0))
                , dfaTrans = frozen
                , dfaShift = shift
                , dfaStart = s
                , dfaAtEnd =
                    PrimArray.primArrayFromList
                      (fmap (\k -> if IntMap.findWithDefault False k ends then 1 else 0) [0 .. n - 1])
                , dfaEmpty = emptyMatch
                }
      _ -> Nothing

data NfaState
  = NfaSet !Charset.Ascii !Int
  | NfaSplit !Int !Int
  | NfaBol !Int
  | NfaEol !Int
  | NfaMatch

data NfaBuilder = NfaBuilder
  { nfaStates :: !(IntMap.IntMap NfaState)
  , nfaCount :: !Int
  }

data Builder s = Builder
  { builderNfa :: !(Array.Array NfaState)
  , builderMark :: !(PrimArray.MutablePrimArray s Int)
  , builderGen :: !(MutVar.MutVar s Int)
  , builderSteps :: !(MutVar.MutVar s Int)
  }

addState :: NfaState -> Nfa Int
addState s = do
  b <- State.get
  let
    k = nfaCount b
  Monad.when (k >= maxNfaStates) (Trans.lift Nothing)
  State.put b{nfaStates = IntMap.insert k s (nfaStates b), nfaCount = k + 1}
  pure k

type Nfa = State.StateT NfaBuilder Maybe

maxDfaStates, maxNfaStates, maxDfaSteps :: Int
maxDfaStates = 256
maxNfaStates = 512
maxDfaSteps = 400000

compileNode :: Parse.Node [Int] -> Int -> Nfa Int
compileNode n next =
  case n of
    Parse.Empty -> pure next
    Parse.Char r ->
      addState (NfaSet (if r < 128 then Charset.asciiSingleton r else Charset.asciiNone) next)
    Parse.Set set -> addState (NfaSet (Charset.charsetAscii set) next)
    Parse.Cat subs -> Monad.foldM (flip compileNode) next (reverse subs)
    Parse.Alt subs ->
      case reverse subs of
        [] -> pure next
        lastSub : earlier -> do
          start <- compileNode lastSub next
          Monad.foldM
            ( \acc sub -> do
                out <- compileNode sub next
                addState (NfaSplit out acc)
            )
            start
            earlier
    Parse.Group _ sub -> compileNode sub next
    Parse.Repeat low high _ sub -> do
      afterOptional <-
        case high of
          Parse.Unbounded -> do
            loop <- addState (NfaSplit 0 next)
            out <- compileNode sub loop
            State.modify' (\b -> b{nfaStates = IntMap.insert loop (NfaSplit out next) (nfaStates b)})
            pure loop
          Parse.Max m ->
            Monad.foldM
              ( \acc _ -> do
                  out <- compileNode sub acc
                  addState (NfaSplit out acc)
              )
              next
              [low .. m - 1]
      Monad.foldM (\acc _ -> compileNode sub acc) afterOptional [1 .. low]
    Parse.Bol -> addState (NfaBol next)
    Parse.Eol -> addState (NfaEol next)
    _ -> Trans.lift Nothing

matchState :: Int
matchState = 0

dfaDead, dfaAccept, dfaBail, dfaFirst :: Int
dfaDead = 0
dfaAccept = 1
dfaBail = 2
dfaFirst = 3

hasMatch :: IntSet.IntSet -> Bool
hasMatch =
  IntSet.member matchState

closure :: Builder s -> Bool -> Bool -> [Int] -> ST.ST s IntSet.IntSet
closure b atStart atEnd from = do
  gen <- fmap (+ 1) (MutVar.readMutVar (builderGen b))
  MutVar.writeMutVar (builderGen b) gen
  let
    go !kept !pops stack =
      case stack of
        [] -> pure (kept, pops)
        k : rest -> do
          seen <- PrimArray.readPrimArray (builderMark b) k
          if seen == gen
            then go kept (pops + 1) rest
            else do
              PrimArray.writePrimArray (builderMark b) k gen
              case Array.indexArray (builderNfa b) k of
                NfaSplit out alt -> go kept (pops + 1) (alt : out : rest)
                NfaBol out -> go kept (pops + 1) (if atStart then out : rest else rest)
                NfaEol out ->
                  if atEnd
                    then go kept (pops + 1) (out : rest)
                    else go (IntSet.insert k kept) (pops + 1) rest
                _ -> go (IntSet.insert k kept) (pops + 1) rest
  (kept, pops) <- go IntSet.empty (0 :: Int) (reverse from)
  MutVar.modifyMutVar' (builderSteps b) (subtract pops)
  pure kept

matchDfa :: Dfa -> Utf8.Bytes -> Int -> Int -> Answer
matchDfa d text lo hi =
  let
    trans = dfaTrans d
    classes = dfaByteClass d
    shift = dfaShift d
    go i s
      | i == hi = if PrimArray.indexPrimArray (dfaAtEnd d) s /= 0 then Matched else NoMatch
      | otherwise =
          let
            c = Utf8.byte text i
            next =
              fromIntegral
                (PrimArray.indexPrimArray trans (Bits.unsafeShiftL s shift Bits..|. fromIntegral (PrimArray.indexPrimArray classes c)))
           in
            if next < dfaFirst then finish next else go (i + 1) next
   in
    if lo == hi
      then if dfaEmpty d then Matched else NoMatch
      else
        if dfaStart d == dfaAccept
          then Matched
          else go lo (dfaStart d)

finish :: Int -> Answer
finish s
  | s == dfaAccept = Matched
  | s == dfaBail = Undecided
  | otherwise = NoMatch
