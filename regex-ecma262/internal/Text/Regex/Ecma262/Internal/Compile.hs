module Text.Regex.Ecma262.Internal.Compile
  ( Op (..)
  , Quick (..)
  , Inst (..)
  , Program (..)
  , compileProgram
  , noMatch
  , maxMemoLoops
  , Direction (..)
  ) where

import Control.Monad qualified as Monad
import Control.Monad.ST qualified as ST
import Control.Monad.Trans.State.Strict qualified as State
import Data.Foldable qualified as Foldable
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Maybe qualified as Maybe
import Data.Primitive.Array qualified as Array
import Data.Primitive.PrimArray qualified as PrimArray

import Text.Regex.Ecma262.Internal.Charset qualified as Charset
import Text.Regex.Ecma262.Internal.Mode qualified as Mode
import Text.Regex.Ecma262.Internal.Parse qualified as Parse
import Text.Regex.Ecma262.Internal.Utf8 qualified as Utf8

data Op
  = OpChar
  | OpCharBack
  | OpSet
  | OpSetBack
  | OpLit
  | OpFail
  | OpBol
  | OpEol
  | OpWordBoundary
  | OpNotWordBoundary
  | OpLineStart
  | OpLineEnd
  | OpWordBoundaryFold
  | OpNotWordBoundaryFold
  | OpJmp
  | OpSplit
  | OpSaveTmp
  | OpCapture
  | OpClear
  | OpBackref
  | OpBackrefBack
  | OpBackrefFold
  | OpBackrefFoldBack
  | OpBackrefUnits
  | OpBackrefUnitsBack
  | OpLook
  | OpLookOk
  | OpLookFail
  | OpRep
  | OpRepLazy
  | OpLoopInit
  | OpLoop
  | OpLoopIter
  | OpMatch
  deriving (Eq, Show)

data Quick = Quick
  { quickAscii :: {-# UNPACK #-} !Charset.Ascii
  , quickOther :: !Bool
  , quickMid :: !Bool
  , quickAtEnd :: !Bool
  }
  deriving (Eq)

data Inst = Inst
  { instOp :: !Op
  , instFlag :: !Bool
  , instR :: !Int
  , instSet :: !Charset.Charset
  , instHasSet :: !Bool
  , instLit :: !(PrimArray.PrimArray Int)
  , instX :: !Int
  , instY :: !Int
  , instA :: !Int
  , instB :: !Int
  , instMin :: !Int
  , instMax :: !Int
  , instNeed :: !Int
  , instIter :: !Int
  , instQuick :: !Quick
  , instM :: !Int
  , instMl0 :: !Int
  , instMl1 :: !Int
  , instMm0 :: !Int
  , instMm1 :: !Int
  }

data Program = Program
  { programInsts :: !(Array.Array Inst)
  , programGroups :: !Int
  , programLoops :: !Int
  , programMode :: !Mode.Mode
  , programAnchored :: !Bool
  , programNeed :: !Int
  , programPoints :: !Int
  , programPrefix :: !(PrimArray.PrimArray Int)
  , programFirst :: !(Maybe Charset.Charset)
  }

compileProgram :: Mode.Mode -> Int -> Tree -> Program
compileProgram mode groupCount parsedRoot =
  let
    simplified = simplify [] parsedRoot
    referenced = backrefGroups simplified
    tracked = IntMap.fromList (zip (filter (`IntSet.member` referenced) [1 .. groupCount]) [0 ..])
    memoizable = IntMap.null tracked
    root = if memoizable then unrollCounted simplified else simplified
    env = Env{envTracked = tracked, envMode = mode, envMemo = memoizable}
    initial =
      Compiler
        { compilerCode = IntMap.empty
        , compilerCount = 0
        , compilerLoopCount = 0
        , compilerOpenLoops = []
        , compilerPointCount = 0
        }
    compiler = State.execState (emit env Forward root *> add_ (op OpMatch)) initial
    raw = Array.arrayFromList (IntMap.elems (compilerCode compiler))
    needs = setNeeds raw
    quicks = setQuick raw
    insts =
      Array.arrayFromList
        . forceAll
        . zipWith
          ( \pc i ->
              i
                { instQuick = Array.indexArray quicks pc
                , instNeed = if instOp i == OpLoop then PrimArray.indexPrimArray needs pc else instNeed i
                }
          )
          [0 ..]
        . IntMap.elems
        $ compilerCode compiler
    anchored = startsAtBol root
    prefix = if anchored then [] else literalPrefix root
    first =
      if anchored || not (null prefix)
        then Nothing
        else case firstChars root of
          Just (pairs, False) -> Just (Charset.fromRanges pairs)
          _ -> Nothing
   in
    Program
      { programInsts = insts
      , programGroups = IntMap.size tracked
      , programLoops = compilerLoopCount compiler
      , programMode = mode
      , programAnchored = anchored
      , programNeed = PrimArray.indexPrimArray needs 0
      , programPoints = compilerPointCount compiler
      , programPrefix = PrimArray.primArrayFromList (take maxPrefix prefix)
      , programFirst = first
      }

type Tree = Parse.Node [Int]

data Env = Env
  { envTracked :: !(IntMap.IntMap Int)
  , envMode :: !Mode.Mode
  , envMemo :: !Bool
  }

data Compiler = Compiler
  { compilerCode :: !(IntMap.IntMap Inst)
  , compilerCount :: !Int
  , compilerLoopCount :: !Int
  , compilerOpenLoops :: ![OpenLoop]
  , compilerPointCount :: !Int
  }

data OpenLoop = OpenLoop
  { openLoopNumber :: !Int
  , openLoopMin :: !Int
  , openLoopSimple :: !Bool
  }

data Direction
  = Forward
  | Backward
  deriving (Eq)

simplify :: [Int] -> Tree -> Tree
simplify inside n =
  let
    recurse = simplify inside
   in
    case n of
      Parse.Backref groups _ ->
        if all (`elem` inside) groups then Parse.Empty else n
      Parse.Cat subs -> Parse.Cat (fmap recurse subs)
      Parse.Alt subs -> Parse.Alt (fmap recurse subs)
      Parse.Group g sub ->
        Parse.Group g (simplify (if g > 0 then g : inside else inside) sub)
      Parse.Look direction sense sub -> Parse.Look direction sense (recurse sub)
      Parse.Repeat low high greed sub ->
        let
          body = recurse sub
         in
          if high /= Parse.Max 0 && not (consumes body)
            then if low == 0 then Parse.Empty else body
            else Parse.Repeat low high greed body
      _ -> n

unrollCounted :: Tree -> Tree
unrollCounted n =
  case n of
    Parse.Cat subs -> Parse.Cat (fmap unrollCounted subs)
    Parse.Alt subs -> Parse.Alt (fmap unrollCounted subs)
    Parse.Group g sub -> Parse.Group g (unrollCounted sub)
    Parse.Look direction sense sub -> Parse.Look direction sense (unrollCounted sub)
    Parse.Repeat low high greed sub ->
      let
        body = unrollCounted sub
        optional inner =
          Parse.Alt (if greed == Parse.Greedy then [inner, Parse.Empty] else [Parse.Empty, inner])
        nested k = optional (if k <= 1 then body else Parse.Cat [body, nested (k - 1)])
        (extra, rest) =
          case high of
            Parse.Unbounded -> (1, [Parse.Repeat 0 Parse.Unbounded greed body])
            Parse.Max m -> (m - low, [nested (m - low) | m > low])
       in
        if blocksMemo low high body && (treeSize body + copyOverhead) * (low + extra) <= maxUnrolled
          then Parse.Cat (replicate low body <> rest)
          else Parse.Repeat low high greed body
    _ -> n

blocksMemo :: Int -> Parse.RepeatMax -> Tree -> Bool
blocksMemo low high body =
  let
    single = Maybe.isJust (singleStep body)
    splitOnly =
      (high == Parse.Unbounded && low <= 1)
        || (low == 0 && high == Parse.Max 1 && not (nullable body))
   in
    high /= Parse.Max 0 && not single && not splitOnly

singleStep :: Tree -> Maybe (Int, Charset.Charset, Bool)
singleStep body =
  case body of
    Parse.Char r -> Just (r, Charset.none, False)
    Parse.Set set | not (Charset.isEmpty set) -> Just (0, set, True)
    _ -> Nothing

treeSize :: Tree -> Int
treeSize n =
  1 + sum (fmap treeSize (Parse.children n))

copyOverhead :: Int
copyOverhead = 3

maxUnrolled :: Int
maxUnrolled = 1024

consumes :: Tree -> Bool
consumes n =
  case n of
    Parse.Char _ -> True
    Parse.Set _ -> True
    Parse.Backref _ _ -> True
    Parse.Cat subs -> any consumes subs
    Parse.Alt subs -> any consumes subs
    Parse.Group _ sub -> consumes sub
    Parse.Repeat _ high _ sub -> high /= Parse.Max 0 && consumes sub
    _ -> False

backrefGroups :: Tree -> IntSet.IntSet
backrefGroups =
  IntSet.fromList . Monad.join . Foldable.toList

emit :: Env -> Direction -> Tree -> Emit ()
emit env direction n =
  let
    back = direction == Backward
   in
    case n of
      Parse.Empty -> pure ()
      Parse.Char r -> add_ (op (if back then OpCharBack else OpChar)){instR = r}
      Parse.Set set ->
        if Charset.isEmpty set
          then add_ (op OpFail)
          else add_ (op (if back then OpSetBack else OpSet)){instSet = set, instHasSet = True}
      Parse.Cat subs ->
        if back
          then Foldable.traverse_ (emit env Backward) (reverse subs)
          else emitForwardCat env subs
      Parse.Alt subs -> emitAlt env direction subs
      Parse.Group g sub ->
        let
          t = slot env g
         in
          if t < 0
            then emit env direction sub
            else do
              add_ (op OpSaveTmp){instA = t}
              emit env direction sub
              add_ (op OpCapture){instA = t, instFlag = back}
      Parse.Bol -> add_ (op OpBol)
      Parse.Eol -> add_ (op OpEol)
      Parse.LineStart -> add_ (op OpLineStart)
      Parse.LineEnd -> add_ (op OpLineEnd)
      Parse.WordBoundary fold ->
        add_ (op (if fold == Parse.CaseInsensitive then OpWordBoundaryFold else OpWordBoundary))
      Parse.NotWordBoundary fold ->
        add_ (op (if fold == Parse.CaseInsensitive then OpNotWordBoundaryFold else OpNotWordBoundary))
      Parse.Backref groups fold ->
        let
          unicode = envMode env == Mode.UnicodeMode
          folded = fold == Parse.CaseInsensitive
          o
            | folded && back = OpBackrefFoldBack
            | folded = OpBackrefFold
            | back = if unicode then OpBackrefBack else OpBackrefUnitsBack
            | otherwise = if unicode then OpBackref else OpBackrefUnits
         in
          Foldable.traverse_ (\g -> add_ (op o){instA = slot env g, instFlag = unicode}) groups
      Parse.Look lookDirection sense sub -> do
        look <- add (op OpLook){instFlag = sense == Parse.Negative}
        outer <- State.gets compilerOpenLoops
        State.modify' (\c -> c{compilerOpenLoops = []})
        emit env (if lookDirection == Parse.Behind then Backward else Forward) sub
        State.modify' (\c -> c{compilerOpenLoops = outer})
        add_ (op (if sense == Parse.Negative then OpLookFail else OpLookOk))
        end <- pcNow
        patch look (\i -> i{instX = end})
      Parse.Repeat low high greed sub -> emitRepeat env direction low high greed sub

type Emit = State.State Compiler

add_ :: Inst -> Emit ()
add_ =
  Monad.void . add

add :: Inst -> Emit Int
add inst = do
  c <- State.get
  let
    pc = compilerCount c
  State.put c{compilerCode = IntMap.insert pc inst (compilerCode c), compilerCount = pc + 1}
  pure pc

op :: Op -> Inst
op o =
  noInst{instOp = o}

noInst :: Inst
noInst =
  Inst
    { instOp = OpFail
    , instFlag = False
    , instR = 0
    , instSet = Charset.none
    , instHasSet = False
    , instLit = PrimArray.emptyPrimArray
    , instX = 0
    , instY = 0
    , instA = 0
    , instB = 0
    , instMin = 0
    , instMax = 0
    , instNeed = 0
    , instIter = 0
    , instQuick = quickNone
    , instM = 0
    , instMl0 = -1
    , instMl1 = -1
    , instMm0 = 0
    , instMm1 = 0
    }

quickNone :: Quick
quickNone = Quick Charset.asciiNone False False False

emitForwardCat :: Env -> [Tree] -> Emit ()
emitForwardCat env subs =
  case subs of
    [] -> pure ()
    first : others ->
      let
        (run, rest) = span (Maybe.isJust . literalChar) subs
        runes = foldMap (Foldable.toList . literalChar) run
       in
        if length run >= 2
          then do
            add_ (op OpLit){instLit = PrimArray.primArrayFromList (foldMap Utf8.encodeRune runes)}
            emitForwardCat env rest
          else do
            emit env Forward first
            emitForwardCat env others

literalChar :: Tree -> Maybe Int
literalChar n =
  case n of
    Parse.Char r | literalRune r -> Just r
    _ -> Nothing

literalRune :: Int -> Bool
literalRune r =
  Utf8.validRune r && r /= Utf8.runeError

emitAlt :: Env -> Direction -> [Tree] -> Emit ()
emitAlt env direction subs =
  let
    go jumps alternatives =
      case alternatives of
        [] -> pure jumps
        [lastSub] -> do
          emit env direction lastSub
          pure jumps
        sub : rest -> do
          inst <- withPoint env (op OpSplit)
          split <- add inst
          enter <- pcNow
          patch split (\i -> i{instX = enter})
          emit env direction sub
          jump <- add (op OpJmp)
          next <- pcNow
          patch split (\i -> i{instY = next})
          go (jump : jumps) rest
   in
    do
      jumps <- go [] subs
      end <- pcNow
      Foldable.traverse_ (\j -> patch j (\i -> i{instX = end})) jumps

withPoint :: Env -> Inst -> Emit Inst
withPoint env inst = do
  c <- State.get
  let
    loops = reverse (compilerOpenLoops c)
    loopAt k = case drop k loops of
      l : _ -> (openLoopNumber l, openLoopMin l)
      [] -> (-1, 0)
    (ml0, mm0) = loopAt 0
    (ml1, mm1) = loopAt 1
  if not (envMemo env) || length loops > maxMemoLoops || not (all openLoopSimple loops)
    then pure inst
    else do
      let
        point = compilerPointCount c + 1
      State.put c{compilerPointCount = point}
      pure inst{instM = point, instMl0 = ml0, instMl1 = ml1, instMm0 = mm0, instMm1 = mm1}

maxMemoLoops :: Int
maxMemoLoops = 2

pcNow :: Emit Int
pcNow =
  State.gets compilerCount

patch :: Int -> (Inst -> Inst) -> Emit ()
patch pc f =
  State.modify' (\c -> c{compilerCode = IntMap.adjust f pc (compilerCode c)})

slot :: Env -> Int -> Int
slot env g =
  if g > 0 then IntMap.findWithDefault (-1) g (envTracked env) else -1

emitRepeat :: Env -> Direction -> Int -> Parse.RepeatMax -> Parse.Greed -> Tree -> Emit ()
emitRepeat env direction low high greed body =
  let
    back = direction == Backward
    lazy = greed == Parse.Lazy
    highInt = case high of
      Parse.Max m -> m
      Parse.Unbounded -> unbounded
    single = singleStep body
    (clearLo, clearHi) = groupRange env body
    emitBody = do
      Monad.when (clearLo >= 0) (add_ (op OpClear){instA = clearLo, instB = clearHi})
      emit env direction body
    split = do
      inst <- withPoint env (op OpSplit)
      add inst
    setSplit at enter leave =
      patch at $ \i ->
        if lazy then i{instX = leave, instY = enter} else i{instX = enter, instY = leave}
    plain = not (nullable body)
   in
    if highInt == 0
      then pure ()
      else case single of
        Just (r, set, hasSet)
          | not back -> do
              inst <-
                withPoint
                  env
                  (op (if lazy then OpRepLazy else OpRep))
                    { instR = r
                    , instSet = set
                    , instHasSet = hasSet
                    , instMin = low
                    , instMax = highInt
                    }
              add_ inst
        _
          | plain && low == 0 && high == Parse.Unbounded -> do
              at <- split
              enter <- pcNow
              emitBody
              add_ (op OpJmp){instX = at}
              leave <- pcNow
              setSplit at enter leave
          | plain && low == 1 && high == Parse.Unbounded -> do
              enter <- pcNow
              emitBody
              at <- split
              leave <- pcNow
              setSplit at enter leave
          | plain && low == 0 && high == Parse.Max 1 -> do
              at <- split
              enter <- pcNow
              emitBody
              leave <- pcNow
              setSplit at enter leave
          | otherwise -> do
              k <- State.gets compilerLoopCount
              State.modify' (\c -> c{compilerLoopCount = k + 1})
              add_ (op OpLoopInit){instA = k}
              let
                simple = high == Parse.Unbounded && low <= 1
                loopInst =
                  (op OpLoop)
                    { instA = k
                    , instMin = low
                    , instMax = highInt
                    , instFlag = lazy
                    , instIter = if back then 0 else minLength body
                    }
              pointed <- if simple then withPoint env loopInst else pure loopInst
              loop <- add pointed
              add_ (op OpLoopIter){instA = k}
              State.modify' (\c -> c{compilerOpenLoops = OpenLoop k low simple : compilerOpenLoops c})
              emitBody
              State.modify' (\c -> c{compilerOpenLoops = drop 1 (compilerOpenLoops c)})
              add_ (op OpJmp){instX = loop}
              end <- pcNow
              patch loop (\i -> i{instX = end})

unbounded :: Int
unbounded = -1

groupRange :: Env -> Tree -> (Int, Int)
groupRange env =
  let
    slots node =
      case node of
        Parse.Group g sub -> filter (>= 0) [slot env g] <> slots sub
        Parse.Cat subs -> foldMap slots subs
        Parse.Alt subs -> foldMap slots subs
        Parse.Repeat _ _ _ sub -> slots sub
        Parse.Look _ _ sub -> slots sub
        _ -> []
   in
    \n -> case slots n of
      [] -> (-1, -1)
      found@(s : rest) -> (Foldable.foldl' min s rest, Foldable.foldl' max s found)

nullable :: Tree -> Bool
nullable n =
  case n of
    Parse.Char _ -> False
    Parse.Set _ -> False
    Parse.Cat subs -> all nullable subs
    Parse.Alt subs -> any nullable subs
    Parse.Group _ sub -> nullable sub
    Parse.Repeat low _ _ sub -> low == 0 || nullable sub
    _ -> True

minLength :: Tree -> Int
minLength n =
  case n of
    Parse.Char r -> if literalRune r then Utf8.runeLength r else 1
    Parse.Set set -> if Charset.isEmpty set then noMatch else 1
    Parse.Cat subs -> min noMatch (Foldable.foldl' (\total sub -> min noMatch (total + minLength sub)) 0 subs)
    Parse.Alt subs -> Foldable.foldl' (\least sub -> min least (minLength sub)) noMatch subs
    Parse.Group _ sub -> minLength sub
    Parse.Repeat low _ _ sub -> timesNeed low (minLength sub)
    _ -> 0

noMatch :: Int
noMatch = 1073741824

timesNeed :: Int -> Int -> Int
timesNeed count need
  | count == 0 || need == 0 = 0
  | need >= noMatch || count >= noMatch `div` need = noMatch
  | otherwise = count * need

setNeeds :: Array.Array Inst -> PrimArray.PrimArray Int
setNeeds insts =
  ST.runST $ do
    let
      count = Array.sizeofArray insts
      inst = Array.indexArray insts
    need <- PrimArray.newPrimArray count
    PrimArray.setPrimArray need 0 count noMatch
    let
      next target weight = fmap (+ weight) (PrimArray.readPrimArray need target)
      needOf pc i =
        case instOp i of
          OpMatch -> pure 0
          OpLookOk -> pure 0
          OpLookFail -> pure 0
          OpFail -> pure noMatch
          OpChar -> next (pc + 1) 1
          OpSet -> next (pc + 1) 1
          OpLit -> next (pc + 1) (PrimArray.sizeofPrimArray (instLit i))
          OpRep -> next (pc + 1) (instMin i)
          OpRepLazy -> next (pc + 1) (instMin i)
          OpJmp
            | instX i < pc && instOp (inst (instX i)) == OpLoop -> next (instX (inst (instX i))) 0
            | instX i < pc && instOp (inst (instX i)) == OpSplit ->
                min <$> next (instX (inst (instX i))) 0 <*> next (instY (inst (instX i))) 0
            | otherwise -> next (instX i) 0
          OpLook -> next (instX i) 0
          OpSplit -> min <$> next (instX i) 0 <*> next (instY i) 0
          OpLoop -> min <$> next (pc + 1) 0 <*> next (instX i) 0
          OpLoopInit ->
            let
              loop = inst (pc + 1)
             in
              next (pc + 1) (timesNeed (instMin loop) (instIter loop))
          _ -> next (pc + 1) 0
      pass changed pc
        | pc < 0 = pure changed
        | otherwise = do
            n <- fmap (min noMatch) (needOf pc (inst pc))
            old <- PrimArray.readPrimArray need pc
            if n < old
              then PrimArray.writePrimArray need pc n *> pass True (pc - 1)
              else pass changed (pc - 1)
      passes = do
        changed <- pass False (count - 1)
        Monad.when changed passes
    passes
    PrimArray.unsafeFreezePrimArray need

setQuick :: Array.Array Inst -> Array.Array Quick
setQuick insts =
  ST.runST $ do
    let
      count = Array.sizeofArray insts
      inst = Array.indexArray insts
    quick <- Array.newArray count quickNone
    let
      q = Array.readArray quick
      quickOf pc i =
        case instOp i of
          OpMatch -> pure quickAll
          OpLookOk -> pure quickAll
          OpLookFail -> pure quickAll
          OpFail -> pure quickNone
          OpChar -> pure (quickRune (instR i))
          OpSet -> pure (quickSet (instSet i))
          OpLit -> pure (quickRune (PrimArray.indexPrimArray (instLit i) 0))
          OpRep -> rep pc i
          OpRepLazy -> rep pc i
          OpJmp
            | instX i < pc && instOp (inst (instX i)) == OpLoop ->
                quickUnion <$> q (instX i) <*> q (instX (inst (instX i)))
            | instX i < pc && instOp (inst (instX i)) == OpSplit -> do
                atHead <- q (instX i)
                enter <- q (instX (inst (instX i)))
                leave <- q (instY (inst (instX i)))
                pure (quickUnion atHead (quickUnion enter leave))
            | otherwise -> q (instX i)
          OpSplit -> quickUnion <$> q (instX i) <*> q (instY i)
          OpLoop -> quickUnion <$> q (pc + 1) <*> q (instX i)
          OpEol -> fmap (\nextQuick -> quickNone{quickAtEnd = quickAtEnd nextQuick}) (q (pc + 1))
          OpLook ->
            if instFlag i
              then q (instX i)
              else quickIntersection <$> q (instX i) <*> q (pc + 1)
          OpCharBack -> pure quickAll
          OpSetBack -> pure quickAll
          OpBackref -> pure quickAll
          OpBackrefBack -> pure quickAll
          OpBackrefFold -> pure quickAll
          OpBackrefFoldBack -> pure quickAll
          OpBackrefUnits -> pure quickAll
          OpBackrefUnitsBack -> pure quickAll
          _ -> q (pc + 1)
      rep pc i = do
        let
          own = if instHasSet i then quickSet (instSet i) else quickRune (instR i)
        if instMin i == 0
          then fmap (quickUnion own) (q (pc + 1))
          else pure own
      pass changed pc
        | pc < 0 = pure changed
        | otherwise = do
            n <- quickOf pc (inst pc)
            old <- q pc
            if n /= old
              then Array.writeArray quick pc n *> pass True (pc - 1)
              else pass changed (pc - 1)
      passes = do
        changed <- pass False (count - 1)
        Monad.when changed passes
    passes
    Array.unsafeFreezeArray quick

quickAll :: Quick
quickAll = Quick Charset.asciiAll True True True

quickRune :: Int -> Quick
quickRune r =
  if r < Utf8.runeSelf
    then quickNone{quickAscii = Charset.asciiSingleton r}
    else quickNone{quickOther = True}

quickSet :: Charset.Charset -> Quick
quickSet set =
  quickNone{quickAscii = Charset.charsetAscii set, quickOther = Charset.hasNonAscii set}

quickUnion :: Quick -> Quick -> Quick
quickUnion a b =
  Quick
    { quickAscii = Charset.asciiUnion (quickAscii a) (quickAscii b)
    , quickOther = quickOther a || quickOther b
    , quickMid = quickMid a || quickMid b
    , quickAtEnd = quickAtEnd a || quickAtEnd b
    }

quickIntersection :: Quick -> Quick -> Quick
quickIntersection a b =
  Quick
    { quickAscii = Charset.asciiIntersection (quickAscii a) (quickAscii b)
    , quickOther = quickOther a && quickOther b
    , quickMid = quickMid a && quickMid b
    , quickAtEnd = quickAtEnd a && quickAtEnd b
    }

startsAtBol :: Tree -> Bool
startsAtBol n =
  case n of
    Parse.Bol -> True
    Parse.Cat (first : _) -> startsAtBol first
    Parse.Alt subs -> all startsAtBol subs
    Parse.Group _ sub -> startsAtBol sub
    Parse.Repeat low _ _ sub -> low >= 1 && startsAtBol sub
    _ -> False

literalPrefix :: Tree -> [Int]
literalPrefix n =
  case n of
    Parse.Char r -> if literalRune r then Utf8.encodeRune r else []
    Parse.Cat subs ->
      let
        (run, rest) = span (Maybe.isJust . literalChar) subs
       in
        case (run, rest) of
          ([], group@(Parse.Group _ _) : _) -> literalPrefix group
          _ -> foldMap (foldMap Utf8.encodeRune . literalChar) run
    Parse.Group _ sub -> literalPrefix sub
    _ -> []

firstChars :: Tree -> Maybe ([(Int, Int)], Bool)
firstChars n =
  case n of
    Parse.Char r -> Just ([(r, r)], False)
    Parse.Set set -> Just (Charset.toRanges set, False)
    Parse.Cat subs ->
      let
        go acc rest =
          case rest of
            [] -> Just (acc, True)
            sub : others -> do
              (pairs, empty) <- firstChars sub
              if empty then go (acc <> pairs) others else Just (acc <> pairs, False)
       in
        go [] subs
    Parse.Alt subs -> do
      found <- traverse firstChars subs
      Just (foldMap fst found, any snd found)
    Parse.Group _ sub -> firstChars sub
    Parse.Repeat low high _ sub ->
      if high == Parse.Max 0
        then Just ([], True)
        else do
          (pairs, empty) <- firstChars sub
          Just (pairs, empty || low == 0)
    Parse.Backref _ _ -> Nothing
    _ -> Just ([], True)

maxPrefix :: Int
maxPrefix = 16

forceAll :: [a] -> [a]
forceAll xs =
  foldr seq () xs `seq` xs
