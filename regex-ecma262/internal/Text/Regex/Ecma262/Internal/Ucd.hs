module Text.Regex.Ecma262.Internal.Ucd
  ( category
  , binary
  , script
  , scriptExtensions
  , isIdStart
  , isIdContinue
  , fold
  , toUpper
  , folding
  ) where

import Data.Map.Strict qualified as Map
import Data.Text qualified as Text

import Text.Regex.Ecma262.Internal.Ranges qualified as Ranges
import Text.Regex.Ecma262.Internal.Ucd.Table qualified as Table
import Text.Regex.Ecma262.Internal.Ucd.Tables qualified as Tables

category :: Text.Text -> Maybe [(Int, Int)]
category name =
  case name of
    "L" -> Just letter
    "LC" -> Just casedLetter
    "M" -> Just mark
    "N" -> Just number
    "P" -> Just punctuation
    "S" -> Just symbol
    "Z" -> Just separator
    "C" -> Just other
    _ -> lookupTable categoryTables name

letter :: [(Int, Int)]
letter = unionOf ["Lu", "Ll", "Lt", "Lm", "Lo"]

unionOf :: [Text.Text] -> [(Int, Int)]
unionOf =
  Ranges.normalize . foldMap (\name -> foldMap Table.tableRanges (Map.lookup name categoryTables))

categoryTables :: Map.Map Text.Text Table.Table
categoryTables = Map.fromList Tables.categoryTables

casedLetter :: [(Int, Int)]
casedLetter = unionOf ["Lu", "Ll", "Lt"]

mark :: [(Int, Int)]
mark = unionOf ["Mn", "Mc", "Me"]

number :: [(Int, Int)]
number = unionOf ["Nd", "Nl", "No"]

punctuation :: [(Int, Int)]
punctuation = unionOf ["Pc", "Pd", "Ps", "Pe", "Pi", "Pf", "Po"]

symbol :: [(Int, Int)]
symbol = unionOf ["Sm", "Sc", "Sk", "So"]

separator :: [(Int, Int)]
separator = unionOf ["Zs", "Zl", "Zp"]

other :: [(Int, Int)]
other = unionOf ["Cc", "Cf", "Cs", "Co", "Cn"]

lookupTable :: Map.Map Text.Text Table.Table -> Text.Text -> Maybe [(Int, Int)]
lookupTable tables name =
  fmap Table.tableRanges (Map.lookup name tables)

binary :: Text.Text -> Maybe [(Int, Int)]
binary name =
  case name of
    "Assigned" -> Just assigned
    _ -> lookupTable binaryTables name

assigned :: [(Int, Int)]
assigned = Ranges.complement (unionOf ["Cn"])

binaryTables :: Map.Map Text.Text Table.Table
binaryTables = Map.fromList Tables.binaryTables

script :: Text.Text -> Maybe [(Int, Int)]
script =
  scriptLike scriptTables

scriptLike :: Map.Map Text.Text Table.Table -> Text.Text -> Maybe [(Int, Int)]
scriptLike tables name =
  case name of
    "Unknown" -> Just unknownScript
    "Zzzz" -> Just unknownScript
    "Katakana_Or_Hiragana" -> Just []
    "Hrkt" -> Just []
    _ -> lookupTable tables name

unknownScript :: [(Int, Int)]
unknownScript = unionOf ["Cn", "Co", "Cs"]

scriptTables :: Map.Map Text.Text Table.Table
scriptTables = Map.fromList Tables.scriptTables

scriptExtensions :: Text.Text -> Maybe [(Int, Int)]
scriptExtensions =
  scriptLike scriptExtensionsTables

scriptExtensionsTables :: Map.Map Text.Text Table.Table
scriptExtensionsTables = Map.fromList Tables.scriptExtensionsTables

isIdStart :: Int -> Bool
isIdStart r =
  any (`Table.tableContains` r) idStartTable

idStartTable :: Maybe Table.Table
idStartTable = Map.lookup "ID_Start" binaryTables

isIdContinue :: Int -> Bool
isIdContinue r =
  any (`Table.tableContains` r) idContinueTable

idContinueTable :: Maybe Table.Table
idContinueTable = Map.lookup "ID_Continue" binaryTables

fold :: Int -> Int
fold r
  | r < 0x80 = if r >= 0x41 && r <= 0x5A then r + 0x20 else r
  | otherwise = Table.caseTableMap Tables.caseFolds r

toUpper :: Int -> Int
toUpper r
  | r < 0x80 = if r >= 0x61 && r <= 0x7A then r - 0x20 else r
  | otherwise = Table.caseTableMap Tables.upperCases r

folding :: [Int]
folding = Table.caseTableChanged Tables.caseFolds
