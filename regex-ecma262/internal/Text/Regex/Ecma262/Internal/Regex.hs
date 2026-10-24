module Text.Regex.Ecma262.Internal.Regex
  ( Regex
  , regexMode
  , Backends (..)
  , compile
  , compileWith
  , CompileError (..)
  , renderCompileError
  , valid
  , isMatch
  , isMatchWith
  , automaton
  , needs
  ) where

import Data.Foldable qualified as Foldable
import Data.Text qualified as Text
import Data.Text.Internal qualified as TextInternal

import Text.Regex.Ecma262.Internal.Compile qualified as Compile
import Text.Regex.Ecma262.Internal.Dfa qualified as Dfa
import Text.Regex.Ecma262.Internal.Mode qualified as Mode
import Text.Regex.Ecma262.Internal.Parse qualified as Parse
import Text.Regex.Ecma262.Internal.Vm qualified as Vm

data Regex = Regex
  { regexMode :: !Mode.Mode
  , regexDfa :: !(Maybe Dfa.Dfa)
  , regexProgram :: Compile.Program
  }

data Backends
  = AllBackends
  | BacktrackOnly
  deriving (Eq, Show)

compile :: Text.Text -> Either CompileError Regex
compile =
  compileWith AllBackends

data CompileError
  = InvalidPattern !Text.Text !Int
  | PatternTooLarge !Parse.Limit
  deriving (Eq, Show)

compileWith :: Backends -> Text.Text -> Either CompileError Regex
compileWith backends source =
  case parseEither source of
    Left err -> Left err
    Right parsed ->
      let
        root = Parse.parsedRoot parsed
        mode = Parse.parsedMode parsed
        regular = not (Parse.parsedNeedsBacktracking parsed) && withinRegexpLimits root
       in
        Right
          Regex
            { regexMode = mode
            , regexDfa = if regular && backends == AllBackends then Dfa.build root else Nothing
            , regexProgram = Compile.compileProgram mode (Parse.parsedGroupCount parsed) root
            }

parseEither :: Text.Text -> Either CompileError Parse.Parsed
parseEither source =
  case Parse.parse Mode.UnicodeMode source of
    Left (Parse.Invalid reason offset) ->
      case Parse.parse Mode.LegacyMode source of
        Left (Parse.Invalid _ _) -> Left (InvalidPattern reason offset)
        legacy -> fromParse legacy
    unicode -> fromParse unicode

fromParse :: Either Parse.ParseError Parse.Parsed -> Either CompileError Parse.Parsed
fromParse result =
  case result of
    Right parsed -> Right parsed
    Left (Parse.OverLimit limit) -> Left (PatternTooLarge limit)
    Left (Parse.Invalid reason offset) -> Left (InvalidPattern reason offset)

withinRegexpLimits :: Parse.Node ref -> Bool
withinRegexpLimits n =
  case n of
    Parse.Repeat low high _ _
      | isCounted low high && overRepeatLimit low high n -> False
    _ -> all withinRegexpLimits (Parse.children n)

overRepeatLimit :: Int -> Parse.RepeatMax -> Parse.Node ref -> Bool
overRepeatLimit low high n =
  let
    highInt = case high of
      Parse.Unbounded -> -1
      Parse.Max m -> m
   in
    low > maxRegexpRepeat
      || highInt > maxRegexpRepeat
      || ((low >= 2 || highInt >= 2) && not (repeatIsValid maxRegexpRepeat n))

isCounted :: Int -> Parse.RepeatMax -> Bool
isCounted low high =
  not
    ((low == 0 && high == Parse.Unbounded) || (low == 1 && high == Parse.Unbounded) || (low == 0 && high == Parse.Max 1))

maxRegexpRepeat :: Int
maxRegexpRepeat = 1000

repeatIsValid :: Int -> Parse.Node ref -> Bool
repeatIsValid limit n =
  case n of
    Parse.Repeat low high _ _
      | isCounted low high ->
          let
            m = case high of
              Parse.Unbounded -> low
              Parse.Max hm -> hm
           in
            high == Parse.Max 0
              || (m <= limit && all (repeatIsValid (if m > 0 then limit `div` m else limit)) (Parse.children n))
    _ -> all (repeatIsValid limit) (Parse.children n)

renderCompileError :: CompileError -> Text.Text
renderCompileError err =
  case err of
    InvalidPattern reason offset -> "invalid pattern: " <> reason <> " at offset " <> Text.show offset
    PatternTooLarge limit -> Parse.renderLimit limit

valid :: Text.Text -> Bool
valid source =
  case Parse.parse Mode.UnicodeMode source of
    Right _ -> True
    Left _ -> False

isMatch :: Regex -> Text.Text -> Bool
isMatch =
  isMatchWith Vm.MemoWhenWorthwhile

isMatchWith :: Vm.MemoStart -> Regex -> Text.Text -> Bool
isMatchWith memoStart regex (TextInternal.Text bytes off len) =
  case regexDfa regex of
    Nothing -> Vm.matchProgram memoStart (regexProgram regex) bytes off (off + len)
    Just dfa ->
      case Dfa.matchDfa dfa bytes off (off + len) of
        Dfa.Matched -> True
        Dfa.NoMatch -> False
        Dfa.Undecided -> Vm.matchProgram memoStart (regexProgram regex) bytes off (off + len)

automaton :: Regex -> Text.Text -> Maybe Bool
automaton regex (TextInternal.Text bytes off len) =
  case fmap (\dfa -> Dfa.matchDfa dfa bytes off (off + len)) (regexDfa regex) of
    Just Dfa.Matched -> Just True
    Just Dfa.NoMatch -> Just False
    _ -> Nothing

needs :: Regex -> (Int, [(Int, Int)])
needs regex =
  let
    program = regexProgram regex
    loops =
      fmap (\i -> (Compile.instNeed i, Compile.instIter i))
        . filter ((== Compile.OpLoop) . Compile.instOp)
        . Foldable.toList
        $ Compile.programInsts program
   in
    (Compile.programNeed program, loops)
