module Text.Regex.Ecma262.Internal.Parse
  ( Node (..)
  , RepeatMax (..)
  , Greed (..)
  , LookDirection (..)
  , LookSense (..)
  , CaseFold (..)
  , Parsed (..)
  , ParseError (..)
  , Limit (..)
  , renderLimit
  , children
  , parse
  , maxPatternLength
  , maxNesting
  , maxSize
  ) where

import Control.Monad qualified as Monad
import Control.Monad.Trans.Class qualified as Trans
import Control.Monad.Trans.State.Strict qualified as State
import Data.Char qualified as Char
import Data.Functor qualified as Functor
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Primitive.PrimArray qualified as PrimArray
import Data.Text qualified as Text
import Data.Text.Unsafe qualified as TextUnsafe

import Text.Regex.Ecma262.Internal.Charset qualified as Charset
import Text.Regex.Ecma262.Internal.Mode qualified as Mode
import Text.Regex.Ecma262.Internal.Ranges qualified as Ranges
import Text.Regex.Ecma262.Internal.Ucd qualified as Ucd
import Text.Regex.Ecma262.Internal.Utf8 qualified as Utf8

data Node ref
  = Empty
  | Char !Int
  | Set !Charset.Charset
  | Cat ![Node ref]
  | Alt ![Node ref]
  | Group !Int !(Node ref)
  | Repeat !Int !RepeatMax !Greed !(Node ref)
  | Bol
  | Eol
  | LineStart
  | LineEnd
  | WordBoundary !CaseFold
  | NotWordBoundary !CaseFold
  | Look !LookDirection !LookSense !(Node ref)
  | Backref !ref !CaseFold
  deriving (Foldable, Functor, Traversable)

data RepeatMax
  = Max !Int
  | Unbounded
  deriving (Eq, Show)

data Greed
  = Greedy
  | Lazy
  deriving (Eq, Show)

data CaseFold
  = CaseSensitive
  | CaseInsensitive
  deriving (Eq, Show)

data LookDirection
  = Ahead
  | Behind
  deriving (Eq, Show)

data LookSense
  = Positive
  | Negative
  deriving (Eq, Show)

data Parsed = Parsed
  { parsedRoot :: !(Node [Int])
  , parsedMode :: !Mode.Mode
  , parsedGroupCount :: !Int
  , parsedNeedsBacktracking :: !Bool
  , parsedSize :: !Int
  }

data ParseError
  = Invalid !Text.Text !Int
  | OverLimit !Limit
  deriving (Eq, Show)

data Limit
  = LengthLimit
  | NestingLimit
  | SizeLimit
  deriving (Eq, Show)

renderLimit :: Limit -> Text.Text
renderLimit limit =
  case limit of
    LengthLimit -> "the pattern is longer than " <> Text.show maxPatternLength <> " bytes"
    NestingLimit -> "the groups of the pattern nest more than " <> Text.show maxNesting <> " deep"
    SizeLimit -> "the pattern has a size of more than " <> Text.show maxSize <> " bytes when it is read"

maxPatternLength :: Int
maxPatternLength = 1048576

maxNesting :: Int
maxNesting = 1000

maxSize :: Int
maxSize = 1048576

parse :: Mode.Mode -> Text.Text -> Either ParseError Parsed
parse mode source =
  if TextUnsafe.lengthWord8 source > maxPatternLength
    then Left (OverLimit LengthLimit)
    else
      let
        points = PrimArray.primArrayFromList (patternPoints mode source)
        (groupCount, hasNames) = scanGroups points
        initial =
          ParserState
            { psPattern = points
            , psMode = mode
            , psPos = 0
            , psDepth = 0
            , psGroupCount = groupCount
            , psHasNames = hasNames
            , psGroups = 0
            , psNames = Map.empty
            , psPath = []
            , psDisjunctions = 0
            , psIcase = False
            , psMultiline = False
            , psDotAll = False
            , psNeedsBacktracking = False
            , psSize = 0
            , psProperties = Map.empty
            }
        whole = do
          root <- disjunction
          st <- State.get
          Monad.when (psPos st < patternLength st)
            $ if at st (psPos st) == 0x29
              then failAt "unmatched ')'"
              else failAt "unexpected character"
          traverse resolve root
        resolve ref =
          case ref of
            Numbered n -> pure [n]
            Named name -> do
              named <- State.gets (Map.lookup name . psNames)
              case named of
                Nothing -> failAt "reference to an unknown group name"
                Just groups -> pure (fmap namedGroupNumber groups)
       in
        case State.runStateT whole initial of
          Left err -> Left err
          Right (root, st) ->
            Right
              Parsed
                { parsedRoot = root
                , parsedMode = mode
                , parsedGroupCount = max (psGroups st) (psGroupCount st)
                , parsedNeedsBacktracking = psNeedsBacktracking st
                , parsedSize = psSize st
                }

data ParserState = ParserState
  { psPattern :: !(PrimArray.PrimArray Int)
  , psMode :: !Mode.Mode
  , psPos :: !Int
  , psDepth :: !Int
  , psGroupCount :: !Int
  , psHasNames :: !Bool
  , psGroups :: !Int
  , psNames :: !(Map.Map Text.Text [NamedGroup])
  , psPath :: ![(Int, Int)]
  , psDisjunctions :: !Int
  , psIcase :: !Bool
  , psMultiline :: !Bool
  , psDotAll :: !Bool
  , psNeedsBacktracking :: !Bool
  , psSize :: !Int
  , psProperties :: !(Map.Map Text.Text Charset.Charset)
  }

data NamedGroup = NamedGroup
  { namedGroupNumber :: !Int
  , namedGroupPath :: ![(Int, Int)]
  }

data Reference
  = Numbered !Int
  | Named !Text.Text

patternPoints :: Mode.Mode -> Text.Text -> [Int]
patternPoints mode =
  let
    split c =
      let
        r = Char.ord c
       in
        if mode == Mode.LegacyMode && r > 0xFFFF
          then [Utf8.highSurrogate r, Utf8.lowSurrogate r]
          else [r]
   in
    foldMap split . Text.unpack

scanGroups :: PrimArray.PrimArray Int -> (Int, Bool)
scanGroups s =
  let
    n = PrimArray.sizeofPrimArray s
    get = PrimArray.indexPrimArray s
    go k inClass count names
      | k >= n = (count, names)
      | otherwise =
          let
            c = get k
           in
            if c == 0x5C
              then go (k + 2) inClass count names
              else
                if c == 0x5B
                  then go (k + 1) True count names
                  else
                    if c == 0x5D
                      then go (k + 1) False count names
                      else
                        if c == 0x28 && not inClass
                          then
                            if k + 1 >= n || get (k + 1) /= 0x3F
                              then go (k + 1) inClass (count + 1) names
                              else
                                if k + 3 < n && get (k + 2) == 0x3C && get (k + 3) /= 0x3D && get (k + 3) /= 0x21
                                  then go (k + 1) inClass (count + 1) True
                                  else go (k + 1) inClass count names
                          else go (k + 1) inClass count names
   in
    go 0 False 0 False

disjunction :: Parser (Node Reference)
disjunction = do
  st <- State.get
  let
    depth = psDepth st + 1
    disjunctionId = psDisjunctions st + 1
    outer = psPath st
  Monad.when (depth > maxNesting) (overLimit NestingLimit)
  State.put st{psDepth = depth, psDisjunctions = disjunctionId, psPath = (disjunctionId, 0) : outer}
  first <- alternative
  let
    rest alternativeId acc = do
      bar <- eat '|'
      if bar
        then do
          charge nodeSize
          State.modify' (\s -> s{psPath = (disjunctionId, alternativeId) : outer})
          sub <- alternative
          rest (alternativeId + 1) (sub : acc)
        else pure (reverse acc)
  next <- peek
  result <-
    if next /= 0x7C
      then pure first
      else fmap Alt (rest 1 [first])
  State.modify' (\s -> s{psPath = outer, psDepth = psDepth s - 1})
  pure result

type Parser = State.StateT ParserState (Either ParseError)

overLimit :: Limit -> Parser a
overLimit =
  Trans.lift . Left . OverLimit

alternative :: Parser (Node Reference)
alternative = do
  let
    go acc = do
      available <- more
      next <- peek
      if available && next /= 0x7C && next /= 0x29
        then do
          t <- term
          go (t : acc)
        else pure (reverse acc)
  terms <- go []
  pure $ case terms of
    [] -> Empty
    [one] -> one
    _ -> Cat terms

more :: Parser Bool
more =
  State.gets (\st -> psPos st < patternLength st)

patternLength :: ParserState -> Int
patternLength =
  PrimArray.sizeofPrimArray . psPattern

peek :: Parser Int
peek =
  State.gets (\st -> if psPos st < patternLength st then at st (psPos st) else -1)

at :: ParserState -> Int -> Int
at st =
  PrimArray.indexPrimArray (psPattern st)

term :: Parser (Node Reference)
term = do
  charge (2 * nodeSize)
  c <- peek
  st <- State.get
  let
    unicode = psMode st == Mode.UnicodeMode
    plain node = pure (node, Quantifiable)
  (atom, quantifiable) <-
    case asciiChar c of
      '^' -> do
        advance 1
        if psMultiline st
          then needBacktracking Functor.$> (LineStart, NotQuantifiable)
          else pure (Bol, NotQuantifiable)
      '$' -> do
        advance 1
        if psMultiline st
          then needBacktracking Functor.$> (LineEnd, NotQuantifiable)
          else pure (Eol, NotQuantifiable)
      '(' -> group
      '.' -> do
        advance 1
        plain (Set (if psDotAll st then Charset.anySet else Charset.dotSet))
      '[' -> do
        node <- characterClass
        plain node
      '\\' -> atomEscape
      '*' -> failAt "nothing to repeat"
      '+' -> failAt "nothing to repeat"
      '?' -> failAt "nothing to repeat"
      '{' -> do
        Monad.when (unicode || looksLikeQuantifier st) (failAt "nothing to repeat")
        advance 1
        node <- charNode 0x7B
        plain node
      bracket
        | bracket == ']' || bracket == '}' -> do
            Monad.when unicode (failAt "lone bracket")
            advance 1
            node <- charNode c
            plain node
      _ -> do
        advance 1
        node <- charNode c
        plain node
  q <- quantifier
  case q of
    Nothing -> pure atom
    Just (low, high, greed) -> do
      Monad.when (quantifiable == NotQuantifiable) (failAt "nothing to repeat")
      pure (Repeat low high greed atom)

data Quantifiable
  = Quantifiable
  | NotQuantifiable
  deriving (Eq)

charge :: Int -> Parser ()
charge bytes = do
  st <- State.get
  let
    size = psSize st + bytes
  State.put st{psSize = size}
  Monad.when (size > maxSize) (overLimit SizeLimit)

nodeSize :: Int
nodeSize = 32

asciiChar :: Int -> Char
asciiChar c =
  if c > 0 && c < 128 then Char.chr c else '\NUL'

advance :: Int -> Parser ()
advance n =
  State.modify' (\st -> st{psPos = psPos st + n})

needBacktracking :: Parser ()
needBacktracking =
  State.modify' (\s -> s{psNeedsBacktracking = True})

group :: Parser (Node Reference, Quantifiable)
group = do
  advance 1
  question <- eat '?'
  if not question
    then fmap (\node -> (node, Quantifiable)) (capture Nothing)
    else do
      colon <- eat ':'
      if colon
        then do
          body <- disjunction
          close
          pure (Group 0 body, Quantifiable)
        else do
          c <- peek
          if c == 0x69 || c == 0x6D || c == 0x73 || c == 0x2D
            then fmap (\node -> (node, Quantifiable)) modifierGroup
            else do
              ahead <- eatEither '=' '!'
              if ahead
                then lookaround Ahead
                else do
                  angle <- eat '<'
                  if not angle
                    then failAt "invalid group"
                    else do
                      behind <- eatEither '=' '!'
                      if behind
                        then lookaround Behind
                        else do
                          name <- groupName
                          here <- currentPath
                          named <- State.gets (Map.findWithDefault [] name . psNames)
                          Monad.unless
                            (all (\other -> separateAlternatives (namedGroupPath other) here) named)
                            (failAt "duplicate group name")
                          fmap (\node -> (node, Quantifiable)) (capture (Just name))

eat :: Char -> Parser Bool
eat c = do
  next <- peek
  if next == Char.ord c
    then do
      advance 1
      pure True
    else pure False

capture :: Maybe Text.Text -> Parser (Node Reference)
capture name = do
  st <- State.get
  let
    groupNumber = psGroups st + 1
  State.put st{psGroups = groupNumber}
  case name of
    Nothing -> pure ()
    Just n -> do
      let
        named = Map.findWithDefault [] n (psNames st)
      charge (rangeSize * (length (psPath st) + length named))
      here <- currentPath
      State.modify'
        (\s -> s{psNames = Map.insert n (named <> [NamedGroup groupNumber here]) (psNames s)})
  body <- disjunction
  close
  pure (Group groupNumber body)

rangeSize :: Int
rangeSize = 8

currentPath :: Parser [(Int, Int)]
currentPath =
  State.gets (reverse . psPath)

close :: Parser ()
close = do
  closed <- eat ')'
  Monad.unless closed (failAt "unterminated group")

failAt :: Text.Text -> Parser a
failAt message = do
  pos <- State.gets psPos
  Trans.lift (Left (Invalid message pos))

modifierGroup :: Parser (Node Reference)
modifierGroup = do
  saved <- State.get
  let
    invalid = failAt "invalid group modifier"
    go removing seen = do
      done <- eat ':'
      if done
        then pure seen
        else do
          c <- peek
          advance 1
          if c == 0x2D
            then do
              Monad.when removing invalid
              go True seen
            else do
              let
                on = not removing
              flag <-
                case asciiChar c of
                  'i' -> State.modify' (\s -> s{psIcase = on}) Functor.$> 'i'
                  'm' -> State.modify' (\s -> s{psMultiline = on}) Functor.$> 'm'
                  's' -> State.modify' (\s -> s{psDotAll = on}) Functor.$> 's'
                  _ -> invalid
              Monad.when (flag `elem` seen) (failAt "repeated group modifier")
              go removing (flag : seen)
  seen <- go False []
  Monad.when (null seen) invalid
  body <- disjunction
  close
  State.modify'
    ( \s ->
        s
          { psIcase = psIcase saved
          , psMultiline = psMultiline saved
          , psDotAll = psDotAll saved
          }
    )
  pure (Group 0 body)

eatEither :: Char -> Char -> Parser Bool
eatEither a b = do
  first <- eat a
  if first then pure True else eat b

lookaround :: LookDirection -> Parser (Node Reference, Quantifiable)
lookaround direction = do
  st <- State.get
  let
    sense = if at st (psPos st - 1) == 0x21 then Negative else Positive
  body <- disjunction
  close
  needBacktracking
  unicode <- isUnicode
  let
    quantifiable =
      if direction == Ahead && not unicode then Quantifiable else NotQuantifiable
  pure (Look direction sense body, quantifiable)

isUnicode :: Parser Bool
isUnicode =
  State.gets ((== Mode.UnicodeMode) . psMode)

groupName :: Parser Text.Text
groupName = do
  let
    invalid = failAt "invalid group name"
    nameChar = do
      c <- peek
      advance 1
      if c == 0x5C
        then do
          isU <- eat 'u'
          Monad.unless isU invalid
          escaped <- unicodeEscape Mode.UnicodeMode
          maybe invalid pure escaped
        else do
          next <- peek
          if c >= 0xD800 && c <= 0xDBFF && next >= 0xDC00 && next <= 0xDFFF
            then do
              advance 1
              pure (Utf8.combineSurrogates c next)
            else pure c
    go count acc = do
      done <- eat '>'
      if done
        then do
          Monad.when (count == 0) invalid
          pure (Text.pack (fmap Char.chr (reverse acc)))
        else do
          available <- more
          Monad.unless available invalid
          c <- nameChar
          let
            allowed =
              if count == 0
                then c == 0x24 || c == 0x5F || Ucd.isIdStart c
                else c == 0x24 || c == 0x200C || c == 0x200D || Ucd.isIdContinue c
          Monad.unless allowed invalid
          go (count + 1 :: Int) (c : acc)
  go 0 []

unicodeEscape :: Mode.Mode -> Parser (Maybe Int)
unicodeEscape mode = do
  start <- getPos
  braced <- if mode == Mode.UnicodeMode then eat '{' else pure False
  if braced
    then do
      digitsStart <- getPos
      let
        go v = do
          c <- peek
          if c >= 0 && hexValue c >= 0
            then do
              advance 1
              go (if v <= Ranges.maxRune then v * 16 + hexValue c else v)
            else pure v
      v <- go 0
      digitsEnd <- getPos
      closed <- if digitsStart == digitsEnd then pure False else eat '}'
      if not closed || v > Ranges.maxRune
        then setPos start Functor.$> Nothing
        else pure (Just v)
    else do
      unit <- hex 4
      case unit of
        Nothing -> pure Nothing
        Just u -> do
          st <- State.get
          let
            pos = psPos st
          if mode == Mode.UnicodeMode
            && u >= 0xD800
            && u <= 0xDBFF
            && pos + 6 <= patternLength st
            && at st pos == 0x5C
            && at st (pos + 1) == 0x75
            then do
              advance 2
              low <- hex 4
              case low of
                Just l | l >= 0xDC00 && l <= 0xDFFF -> pure (Just (Utf8.combineSurrogates u l))
                _ -> setPos pos Functor.$> Just u
            else pure (Just u)

getPos :: Parser Int
getPos =
  State.gets psPos

hexValue :: Int -> Int
hexValue c
  | c >= 0x30 && c <= 0x39 = c - 0x30
  | c >= 0x61 && c <= 0x66 = c - 0x61 + 10
  | c >= 0x41 && c <= 0x46 = c - 0x41 + 10
  | otherwise = -1

setPos :: Int -> Parser ()
setPos pos =
  State.modify' (\st -> st{psPos = pos})

hex :: Int -> Parser (Maybe Int)
hex n = do
  st <- State.get
  let
    start = psPos st
    digits = fmap (hexValue . at st) [start .. start + n - 1]
  if start + n > patternLength st || any (< 0) digits
    then pure Nothing
    else do
      advance n
      pure (Just (List.foldl' (\v d -> v * 16 + d) 0 digits))

separateAlternatives :: [(Int, Int)] -> [(Int, Int)] -> Bool
separateAlternatives a b =
  case (a, b) of
    ((da, aa) : restA, (db, ab) : restB)
      | da == db -> aa /= ab || separateAlternatives restA restB
    _ -> False

characterClass :: Parser (Node Reference)
characterClass = do
  advance 1
  negated <- eat '^'
  let
    chargeRead count charged = do
      charge (rangeSize * (count - charged))
      pure count
    loop pieces count charged = do
      available <- more
      Monad.unless available (failAt "unterminated character class")
      finished <- eat ']'
      if finished
        then pure (pieces, count, charged)
        else do
          chargedNow <- chargeRead count charged
          first <- classAtom
          st <- State.get
          let
            pos = psPos st
            add ranges = (ranges : pieces, count + length ranges)
          if pos < patternLength st
            && at st pos == 0x2D
            && pos + 1 < patternLength st
            && at st (pos + 1) /= 0x5D
            then do
              advance 1
              second <- classAtom
              case (first, second) of
                (Point lo, Point hi) -> do
                  Monad.when (hi < lo) (failAt "range out of order in character class")
                  uncurry loop (add [(lo, hi)]) chargedNow
                _ -> do
                  unicode <- isUnicode
                  Monad.when unicode (failAt "invalid character class range")
                  let
                    ranges = classAtomRanges first <> [(0x2D, 0x2D)] <> classAtomRanges second
                    withDash = classAtomRanges second : [(0x2D, 0x2D)] : classAtomRanges first : pieces
                  loop withDash (count + length ranges) chargedNow
            else uncurry loop (add (classAtomRanges first)) chargedNow
  (pieces, count, charged) <- loop [] 0 0
  _ <- chargeRead count charged
  icase <- State.gets psIcase
  let
    set = Charset.fromRanges (Monad.join (reverse pieces))
  folded <- if icase then foldSet set else pure set
  pure (Set (if negated then Charset.complement folded else folded))

data ClassAtom
  = Point !Int
  | ClassSet !Charset.Charset

classAtom :: Parser ClassAtom
classAtom = do
  c <- peek
  advance 1
  if c /= 0x5C
    then pure (Point c)
    else do
      available <- more
      Monad.unless available (failAt "\\ at end of source")
      e <- peek
      if e == 0x62
        then advance 1 Functor.$> Point 0x08
        else do
          escaped <- classEscape e
          case escaped of
            Just set -> pure (ClassSet set)
            Nothing -> fmap Point (characterEscape InClass)

data ClassPosition
  = InClass
  | OutsideClass
  deriving (Eq)

classEscape :: Int -> Parser (Maybe Charset.Charset)
classEscape c = do
  st <- State.get
  let
    taking set = advance 1 Functor.$> Just set
    unicode = psMode st == Mode.UnicodeMode
  case asciiChar c of
    'd' -> taking Charset.digitSet
    'D' -> taking Charset.notDigitSet
    'w' -> taking (if unicode && psIcase st then Charset.foldWordSet else Charset.wordSet)
    'W' -> taking (if unicode && psIcase st then Charset.notFoldWordSet else Charset.notWordSet)
    's' -> taking Charset.spaceSet
    'S' -> taking Charset.notSpaceSet
    letter
      | (letter == 'p' || letter == 'P') && unicode -> do
          advance 1
          opened <- eat '{'
          Monad.unless opened (failAt "invalid property name")
          start <- getPos
          let
            toBrace = do
              next <- peek
              available <- more
              Monad.when (available && next /= 0x7D) (advance 1 *> toBrace)
          toBrace
          end <- getPos
          now <- State.get
          let
            expr = Text.pack (fmap (Char.chr . at now) [start .. end - 1])
          closed <- eat '}'
          Monad.unless closed (failAt "invalid property name")
          set <-
            case Map.lookup expr (psProperties now) of
              Just set -> pure set
              Nothing ->
                case Charset.propertyRanges expr of
                  Nothing -> failAt "invalid property name"
                  Just ranges -> do
                    let
                      set = Charset.fromRanges ranges
                    State.modify' (\s -> s{psProperties = Map.insert expr set (psProperties s)})
                    pure set
          if letter == 'P'
            then do
              charge (rangeSize * Charset.rangeCount set)
              pure (Just (Charset.complement set))
            else pure (Just set)
    _ -> pure Nothing

characterEscape :: ClassPosition -> Parser Int
characterEscape position = do
  c <- peek
  advance 1
  unicode <- isUnicode
  hasNames <- State.gets psHasNames
  let
    inClass = position == InClass
  case asciiChar c of
    'f' -> pure 0x0C
    'n' -> pure 0x0A
    'r' -> pure 0x0D
    't' -> pure 0x09
    'v' -> pure 0x0B
    'c' -> do
      l <- peek
      if isAsciiLetter l || (not unicode && inClass && (isDigit l || l == 0x5F))
        then advance 1 Functor.$> l `mod` 32
        else do
          Monad.when unicode (failAt "invalid control escape")
          advance (-1)
          pure 0x5C
    '0' -> do
      next <- peek
      if next >= 0 && isDigit next
        then do
          Monad.when unicode (failAt "invalid decimal escape")
          advance (-1)
          legacyOctal
        else pure 0
    'x' -> do
      h <- hex 2
      case h of
        Just v -> pure v
        Nothing -> do
          Monad.when unicode (failAt "invalid \\x escape")
          pure 0x78
    'u' -> do
      u <- unicodeEscape (if unicode then Mode.UnicodeMode else Mode.LegacyMode)
      case u of
        Just v -> pure v
        Nothing -> do
          Monad.when unicode (failAt "invalid \\u escape")
          pure 0x75
    _ ->
      if c >= 0x31 && c <= 0x39 && not unicode && inClass
        then advance (-1) *> legacyOctal
        else do
          Monad.when
            (unicode && not (syntaxCharacter c) && not (inClass && c == 0x2D))
            (failAt "invalid escape")
          Monad.when (c == 0x6B && hasNames) (failAt "invalid escape")
          pure c

isAsciiLetter :: Int -> Bool
isAsciiLetter c =
  (c >= 0x61 && c <= 0x7A) || (c >= 0x41 && c <= 0x5A)

isDigit :: Int -> Bool
isDigit c =
  c >= 0x30 && c <= 0x39

legacyOctal :: Parser Int
legacyOctal = do
  let
    go v k = do
      c <- peek
      if k < (3 :: Int) && c >= 0x30 && c <= 0x37 && v * 8 + (c - 0x30) <= 0o377
        then advance 1 *> go (v * 8 + (c - 0x30)) (k + 1)
        else pure (v, k)
  (v, k) <- go 0 0
  if k == 0
    then do
      d <- peek
      advance 1
      pure d
    else pure v

syntaxCharacter :: Int -> Bool
syntaxCharacter c =
  asciiChar c `elem` ("^$\\.*+?()[]{}|/" :: String)

classAtomRanges :: ClassAtom -> [(Int, Int)]
classAtomRanges atom =
  case atom of
    Point c -> [(c, c)]
    ClassSet set -> Charset.toRanges set

foldSet :: Charset.Charset -> Parser Charset.Charset
foldSet set = do
  mode <- State.gets psMode
  let
    (closed, variants) = Charset.foldClosureCounted mode set
  charge (variants + rangeSize * Charset.rangeCount closed)
  pure closed

atomEscape :: Parser (Node Reference, Quantifiable)
atomEscape = do
  advance 1
  available <- more
  Monad.unless available (failAt "\\ at end of source")
  c <- peek
  st <- State.get
  let
    unicode = psMode st == Mode.UnicodeMode
    fold = if psIcase st then CaseInsensitive else CaseSensitive
  if c == 0x62 || c == 0x42
    then do
      advance 1
      let
        boundaryFold = if unicode && psIcase st then CaseInsensitive else CaseSensitive
      Monad.when (boundaryFold == CaseInsensitive) needBacktracking
      pure
        ( if c == 0x42 then NotWordBoundary boundaryFold else WordBoundary boundaryFold
        , NotQuantifiable
        )
    else
      if c == 0x6B
        then
          if unicode || psHasNames st
            then do
              advance 1
              angle <- eat '<'
              Monad.unless angle (failAt "invalid named reference")
              name <- groupName
              needBacktracking
              pure (Backref (Named name) fold, Quantifiable)
            else do
              advance 1
              node <- charNode 0x6B
              pure (node, Quantifiable)
        else
          if c >= 0x31 && c <= 0x39
            then do
              start <- getPos
              n <- number
              if n <= psGroupCount st
                then do
                  needBacktracking
                  pure (Backref (Numbered n) fold, Quantifiable)
                else do
                  Monad.when unicode (failAt "reference to a group that does not exist")
                  setPos start
                  octal <- legacyOctal
                  node <- charNode octal
                  pure (node, Quantifiable)
            else do
              escaped <- classEscape c
              node <-
                case escaped of
                  Just set -> setNode set
                  Nothing -> do
                    point <- characterEscape OutsideClass
                    charNode point
              pure (node, Quantifiable)

charNode :: Int -> Parser (Node Reference)
charNode c = do
  st <- State.get
  if psIcase st
    then
      let
        set = Charset.foldClosure (psMode st) (Charset.singleton c)
       in
        pure $ case Charset.toRanges set of
          [(lo, hi)] | lo == hi -> Char c
          _ -> Set set
    else pure (Char c)

number :: Parser Int
number = do
  start <- getPos
  let
    go v = do
      c <- peek
      if isDigit c
        then do
          advance 1
          go (min maxCount (v * 10 + (c - 0x30)))
        else pure v
  v <- go 0
  end <- getPos
  Monad.when (start == end) (failAt "expected a number")
  pure v

maxCount :: Int
maxCount = 0x7FFFFFFF

setNode :: Charset.Charset -> Parser (Node Reference)
setNode set = do
  icase <- State.gets psIcase
  if icase
    then fmap Set (foldSet set)
    else pure (Set set)

looksLikeQuantifier :: ParserState -> Bool
looksLikeQuantifier st =
  let
    n = patternLength st
    digitsFrom k =
      if k < n && isDigit (at st k) then digitsFrom (k + 1) else k
    start = psPos st + 1
    afterFirst = digitsFrom start
   in
    afterFirst /= start
      && afterFirst < n
      && ( at st afterFirst == 0x7D
             || ( at st afterFirst == 0x2C
                    && let
                         afterSecond = digitsFrom (afterFirst + 1)
                        in
                         afterSecond < n && at st afterSecond == 0x7D
                )
         )

quantifier :: Parser (Maybe (Int, RepeatMax, Greed))
quantifier = do
  st <- State.get
  c <- peek
  bounds <-
    case asciiChar c of
      '*' -> advance 1 Functor.$> Just (0, Unbounded)
      '+' -> advance 1 Functor.$> Just (1, Unbounded)
      '?' -> advance 1 Functor.$> Just (0, Max 1)
      '{'
        | looksLikeQuantifier st -> do
            advance 1
            first <- getPos
            low <- number
            firstEnd <- getPos
            comma <- eat ','
            high <-
              if not comma
                then pure (Max low)
                else do
                  next <- peek
                  if next == 0x7D
                    then pure Unbounded
                    else do
                      second <- getPos
                      high <- number
                      secondEnd <- getPos
                      now <- State.get
                      Monad.when
                        (greaterAsWritten now first firstEnd second secondEnd)
                        (failAt "numbers out of order in quantifier")
                      pure (Max high)
            closed <- eat '}'
            Monad.unless closed (failAt "malformed quantifier")
            pure (Just (low, high))
      _ -> pure Nothing
  case bounds of
    Nothing -> pure Nothing
    Just (low, high) -> do
      lazy <- eat '?'
      pure (Just (low, high, if lazy then Lazy else Greedy))

greaterAsWritten :: ParserState -> Int -> Int -> Int -> Int -> Bool
greaterAsWritten st aStart aEnd bStart bEnd =
  let
    skipZeros k limit = if k < limit && at st k == 0x30 then skipZeros (k + 1) limit else k
    a = skipZeros aStart aEnd
    b = skipZeros bStart bEnd
    digitsA = fmap (at st) [a .. aEnd - 1]
    digitsB = fmap (at st) [b .. bEnd - 1]
   in
    if aEnd - a /= bEnd - b
      then aEnd - a > bEnd - b
      else digitsA > digitsB

children :: Node ref -> [Node ref]
children n =
  case n of
    Cat subs -> subs
    Alt subs -> subs
    Group _ sub -> [sub]
    Repeat _ _ _ sub -> [sub]
    Look _ _ sub -> [sub]
    _ -> []
