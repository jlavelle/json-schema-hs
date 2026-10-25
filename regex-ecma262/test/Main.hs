module Main
  ( main
  ) where

import Control.Exception qualified as Exception
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Bits qualified as Bits
import Data.ByteString qualified as BS
import Data.Char qualified as Char
import Data.Foldable qualified as Foldable
import Data.List qualified as List
import Data.Maybe qualified as Maybe
import Data.Text qualified as Text
import Data.Vector qualified as Vector
import System.Timeout qualified as Timeout
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit qualified as HUnit

import Text.Regex.Ecma262.Internal.Compile qualified as Compile
import Text.Regex.Ecma262.Internal.Mode qualified as Mode
import Text.Regex.Ecma262.Internal.Parse qualified as Parse
import Text.Regex.Ecma262.Internal.Regex qualified as Regex
import Text.Regex.Ecma262.Internal.Vm qualified as Vm

main :: IO ()
main = do
  cases <- loadJson "test/data/v8_cases.json"
  oracle <- loadJson "test/data/v8_oracle.json"
  sizes <- loadJson "test/data/size_cases.json"
  Tasty.defaultMain . Tasty.testGroup "regex-ecma262"
    $ [ Tasty.testGroup "V8 cases" (fmap (casesGroup cases) caseGroups)
      , Tasty.testGroup
          "V8 oracle"
          [ HUnit.testCase "hand-written patterns" (oracleHandWritten oracle)
          , HUnit.testCase "patterns with flags" (oracleFlagged oracle)
          , HUnit.testCase "generated patterns" (oracleGenerated oracle)
          ]
      , HUnit.testCase "sizes" (sizeCases sizes)
      , HUnit.testCase "needs" needCases
      , HUnit.testCase "unrolled counts" unrolledCases
      , HUnit.testCase "backtracking" backtrackingCases
      , HUnit.testCase "counts on non-ASCII texts" countsOnNonAscii
      ]

loadJson :: FilePath -> IO Aeson.Value
loadJson path = do
  bytes <- BS.readFile path
  either (fail . ((path <> ": ") <>)) pure (Aeson.eitherDecodeStrict bytes)

casesGroup :: Aeson.Value -> Text.Text -> Tasty.TestTree
casesGroup cases name =
  HUnit.testCase (Text.unpack name) $ do
    let
      group = items name cases
      checkCase c =
        let
          p = text "p" c
          (verdict, compiled) = checkVerdict p (flag "u" c) (flag "l" c)
         in
          verdict
            <> foldMap
              (\re -> foldMap (\t -> checkAnswer p re (text "t" t) (flag "want" t)) (items "tests" c))
              compiled
    HUnit.assertBool ("no cases in " <> Text.unpack name) (not (null group))
    assertNone (foldMap checkCase group)

items :: Text.Text -> Aeson.Value -> [Aeson.Value]
items name value =
  case member name value of
    Just (Aeson.Array a) -> Vector.toList a
    _ -> []

member :: Text.Text -> Aeson.Value -> Maybe Aeson.Value
member name value =
  case value of
    Aeson.Object o -> KeyMap.lookup (Key.fromText name) o
    _ -> Nothing

text :: Text.Text -> Aeson.Value -> Text.Text
text name value =
  case member name value of
    Just (Aeson.String s) -> s
    _ -> ""

checkVerdict :: Text.Text -> Bool -> Bool -> (Failures, Maybe (Regex.Regex, Regex.Regex))
checkVerdict p validUnicode validLegacy =
  let
    shown = show p
    validity =
      if Regex.valid p /= validUnicode
        then failure ("valid " <> shown <> " = " <> show (not validUnicode) <> ", V8 says " <> show validUnicode)
        else mempty
   in
    case (Regex.compile p, Regex.compileWith Regex.BacktrackOnly p) of
      (Left err, _) ->
        ( validity
            <> if validUnicode || validLegacy
              then failure ("compile " <> shown <> ": " <> Text.unpack (Regex.renderCompileError err) <> ", V8 says valid")
              else mempty
        , Nothing
        )
      (Right re, Right backtrack)
        | validUnicode || validLegacy ->
            ( validity
                <> if (Regex.regexMode re == Mode.UnicodeMode) /= validUnicode
                  then failure ("mode of " <> shown <> " = " <> show (Regex.regexMode re) <> ", V8 valid with u " <> show validUnicode)
                  else mempty
            , Just (re, backtrack)
            )
      _ -> (validity <> failure ("compile " <> shown <> " succeeds, V8 says invalid"), Nothing)

newtype Failures = Failures [String]
  deriving newtype (Monoid, Semigroup)

failure :: String -> Failures
failure message =
  Failures [message]

flag :: Text.Text -> Aeson.Value -> Bool
flag name value =
  case member name value of
    Just (Aeson.Bool b) -> b
    _ -> False

checkAnswer :: Text.Text -> (Regex.Regex, Regex.Regex) -> Text.Text -> Bool -> Failures
checkAnswer p (re, backtrack) s want =
  let
    one label got =
      if got /= want
        then failure (show p <> " on " <> show s <> " (" <> label <> "): got " <> show got <> ", V8 says " <> show want)
        else mempty
   in
    one "as compiled" (Regex.isMatch re s)
      <> one "backtracking" (Regex.isMatch backtrack s)
      <> one "memo from the start" (Regex.isMatchWith Vm.MemoFromTheStart backtrack s)
      <> foldMap (one "automaton") (Regex.automaton re s)

assertNone :: Failures -> HUnit.Assertion
assertNone (Failures found) =
  case found of
    [] -> pure ()
    _ ->
      HUnit.assertFailure
        (show (length found) <> " failures:\n" <> List.intercalate "\n" (take 40 found))

caseGroups :: [Text.Text]
caseGroups = ["units", "loops", "fast", "needs", "quick", "memo", "classK", "counts", "properties", "names"]

oracleHandWritten :: Aeson.Value -> HUnit.Assertion
oracleHandWritten oracle = do
  let
    inputs = texts "inputs" oracle
    checkHand h =
      let
        p = text "p" h
        (verdict, compiled) = checkVerdict p (flag "u" h) (flag "l" h)
       in
        verdict
          <> if p `elem` v8ModifierBugs
            then mempty
            else foldMap (\re -> checkAnswers p re inputs (text "m" h)) compiled
  assertNone (foldMap checkHand (items "hand" oracle))

texts :: Text.Text -> Aeson.Value -> [Text.Text]
texts name =
  Maybe.mapMaybe
    ( \v -> case v of
        Aeson.String s -> Just s
        _ -> Nothing
    )
    . items name

v8ModifierBugs :: [Text.Text]
v8ModifierBugs =
  ["^(?i:\\u212a)$", "^(?i:\\u017f)$", "^(?i:\\u1e9e)$", "^(?i:\\u03bc)$", "^(?i:\\u03bc)\\&?$", "(?i:\\Bs)"]

checkAnswers :: Text.Text -> (Regex.Regex, Regex.Regex) -> [Text.Text] -> Text.Text -> Failures
checkAnswers p re inputs bits =
  Foldable.fold (zipWith (\k s -> checkAnswer p re s (bit bits k)) [0 ..] inputs)

bit :: Text.Text -> Int -> Bool
bit hex k =
  let
    digit = Char.digitToInt (Text.index hex (k `div` 4))
   in
    Bits.testBit digit (3 - k `mod` 4)

oracleFlagged :: Aeson.Value -> HUnit.Assertion
oracleFlagged oracle = do
  let
    inputs = texts "inputs" oracle
    checkFlagged f =
      let
        p = "(?" <> text "f" f <> ":" <> text "p" f <> ")"
        (verdict, compiled) = checkVerdict p (flag "u" f) (not (flag "u" f))
       in
        verdict <> foldMap (\re -> checkAnswers p re inputs (text "m" f)) compiled
  assertNone (foldMap checkFlagged (items "flagged" oracle))

oracleGenerated :: Aeson.Value -> HUnit.Assertion
oracleGenerated oracle = do
  let
    verdicts = Text.unpack (text "verdicts" oracle)
    randomMatches = texts "randomMatches" oracle
    shortInputs = texts "shortInputs" oracle
    checkGenerated (k, p, verdict) =
      let
        (found, compiled) = checkVerdict p (verdict == '1') (verdict == '2')
       in
        found
          <> case drop k randomMatches of
            bits : _ -> foldMap (\re -> checkAnswers p re shortInputs bits) compiled
            [] -> mempty
  HUnit.assertEqual "verdicts" 60000 (length verdicts)
  assertNone (foldMap checkGenerated (zip3 [0 ..] (generatedPatterns (length verdicts)) verdicts))

generatedPatterns :: Int -> [Text.Text]
generatedPatterns =
  let
    alphabet = "ab0\\\\^$.*+?()[]{}|-,:=!<>dwsbpkuxc1L" :: Text.Text
    draws = drop 1 (iterate xorshift 0x9e3779b9)
    patterns seeds k =
      case seeds of
        n : rest
          | k > 0 ->
              let
                (picks, later) = splitAt (1 + n `mod` 8) rest
               in
                Text.pack (fmap (\r -> Text.index alphabet (r `mod` Text.length alphabet)) picks)
                  : patterns later (k - 1 :: Int)
        _ -> []
   in
    patterns draws

xorshift :: Int -> Int
xorshift s =
  let
    a = Bits.xor s (Bits.shiftL s 13) Bits..&. 0xFFFFFFFF
    b = Bits.xor a (Bits.shiftR a 17)
   in
    Bits.xor b (Bits.shiftL b 5) Bits..&. 0xFFFFFFFF

sizeCases :: Aeson.Value -> HUnit.Assertion
sizeCases sizes =
  let
    number name c =
      case member name c of
        Just (Aeson.Number n) -> round n :: Int
        _ -> minBound
    sizeOf mode p =
      either (const (-1)) Parse.parsedSize (Parse.parse mode p)
    checkSize c =
      let
        p = text "p" c
        under label mode =
          let
            got = sizeOf mode p
            want = number label c
           in
            if got /= want
              then failure ("size of " <> show p <> " (" <> Text.unpack label <> ") = " <> show got <> ", want " <> show want)
              else mempty
       in
        under "u" Mode.UnicodeMode <> under "l" Mode.LegacyMode
   in
    case sizes of
      Aeson.Array cs -> assertNone (foldMap checkSize (Vector.toList cs))
      _ -> HUnit.assertFailure "size_cases.json is not an array"

needCases :: HUnit.Assertion
needCases =
  let
    none = Compile.noMatch
    cases =
      [ ("abc", 3, [], [])
      , ("a+b{2,}c?", 3, [], [])
      , ("(?:a|bc)d", 2, [], [])
      , ("(?=abc)a", 1, [], [])
      , ("(?<=abc)d", 1, [], [])
      , ("\\u00e9{2}\\u{10400}", 3, [], [])
      , ("(?:a|){3}xy$", 2, [2], [0])
      , ("(?:a|){3}", 0, [0], [0])
      , ("(?:(?:a|){2}b){2}c", 3, [1, 2], [1, 0])
      , ("(?=(?:a|){2}xyz)(?:b|){2}c", 1, [3, 1], [0, 0])
      , ("(?<=(?:ab|){2}c)(?:d|){2}ef", 2, [0, 2], [0, 0])
      , ("(?!(?:a|){2}xx)(?:(x)|){2}\\1y", 1, [2, 1], [0, 0])
      , ("(?:a|){2}[]", none, [none], [0])
      , ("(?:(?:a|){2}|bcd)e", 1, [1], [0])
      , ("(?:a|b){5}c", 6, [1], [1])
      , ("(?:ab|cde){3,}?x", 7, [1], [2])
      , ("(?:a{1000}){1000}", 1000000, [0], [1000])
      , ("(?:\\u00e9|xyz){2}", 4, [0], [2])
      , ("(?:a|b){2147483647}c", none, [1], [1])
      , ("(?:(?:a|bc){2}d){3}", 9, [0, 1], [3, 1])
      , ("(?:ab){2147483647}[]", none, [none], [2])
      , ("(?:(?:ab){2147483647}[]){2147483647}[]", none, [none, none], [none, 2])
      , ("(?<=(?:ab|c){2}d)e", 1, [0], [0])
      , ("(?:a|(?=(?:b|c){3}))x{2}", 2, [0], [1])
      ] ::
        [(Text.Text, Int, [Int], [Int])]
    check (counted, need, loops, iterations) =
      let
        p = keepCounters counted
       in
        checkNeeds p (need, zip loops iterations)
   in
    assertNone (foldMap check cases)

keepCounters :: Text.Text -> Text.Text
keepCounters p =
  "()\\1" <> p

checkNeeds :: Text.Text -> (Int, [(Int, Int)]) -> Failures
checkNeeds p want =
  case Regex.compileWith Regex.BacktrackOnly p of
    Left err -> failure ("compile " <> show p <> ": " <> Text.unpack (Regex.renderCompileError err))
    Right re ->
      let
        got = Regex.needs re
       in
        if got /= want
          then failure (show p <> ": needs " <> show got <> ", want " <> show want)
          else mempty

unrolledCases :: HUnit.Assertion
unrolledCases =
  assertNone
    . foldMap (uncurry checkNeeds)
    $ [ ("(?:a|b){5}c", (6, []))
      , ("(?:ab|cde){3,}?x", (7, []))
      , ("(?:(?:a|bc){2}d){3}", (9, []))
      , ("(?:a|){0,3}x", (1, []))
      , ("(?:a|b){2000}c", (2001, [(1, 1)]))
      ]

backtrackingCases :: HUnit.Assertion
backtrackingCases =
  assertNone
    . foldMap (uncurry checkBacktracking)
    $ [ ("(?:ab|c){5}x", Regex.Memoized)
      , ("(?:(?:ab|c){8}){8}", Regex.Memoized)
      , ("(?:(?:ab|c){64}){64}", Regex.Unmemoized)
      , ("(?:a|b){2000}c", Regex.Unmemoized)
      , ("(a)\\1|(?:b|c)*", Regex.Unmemoized)
      ]

checkBacktracking :: Text.Text -> Regex.Backtracking -> Failures
checkBacktracking p want =
  case Regex.compile p of
    Left err -> failure ("compile " <> show p <> ": " <> Text.unpack (Regex.renderCompileError err))
    Right re ->
      let
        got = Regex.backtracking re
       in
        if got /= want
          then failure (show p <> ": " <> show got <> ", want " <> show want)
          else mempty

countsOnNonAscii :: HUnit.Assertion
countsOnNonAscii = do
  let
    nonAscii = "\252" <> Text.replicate 60 "a"
    answer p =
      case Regex.compile p of
        Left err -> HUnit.assertFailure ("compile " <> show p <> ": " <> Text.unpack (Regex.renderCompileError err))
        Right re -> do
          got <- Timeout.timeout 2000000 (Exception.evaluate (Regex.isMatch re nonAscii))
          HUnit.assertEqual (show p) (Just False) got
  Foldable.traverse_
    answer
    [ "^(?:[\\s\\S]{1,4}){3,}x$"
    , "^(?:[\\s\\S]+){3,}x$"
    , "^(?:[\\s\\S]{1,4}){0,64}x$"
    , "^(?:[\\s\\S]?){20}x$"
    ]
