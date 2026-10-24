module Text.Regex.Ecma262.Internal.Charset
  ( Ascii
  , asciiHas
  , asciiNone
  , asciiAll
  , asciiSingleton
  , asciiUnion
  , asciiIntersection
  , asciiComplement
  , Charset
  , charsetAscii
  , fromRanges
  , singleton
  , toRanges
  , rangeCount
  , complement
  , contains
  , isEmpty
  , hasNonAscii
  , none
  , dotSet
  , anySet
  , digitSet
  , notDigitSet
  , wordSet
  , notWordSet
  , spaceSet
  , notSpaceSet
  , foldWordSet
  , notFoldWordSet
  , isWordByte
  , isFoldWordRune
  , propertyRanges
  , canonicalize
  , foldClosureCounted
  , foldClosure
  ) where

import Data.Bifoldable qualified as Bifoldable
import Data.Bifunctor qualified as Bifunctor
import Data.Bits qualified as Bits
import Data.Foldable qualified as Foldable
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Map.Strict qualified as Map
import Data.Primitive.PrimArray qualified as PrimArray
import Data.Text qualified as Text
import Data.Word qualified as Word

import Text.Regex.Ecma262.Internal.Mode qualified as Mode
import Text.Regex.Ecma262.Internal.Ranges qualified as Ranges
import Text.Regex.Ecma262.Internal.Ucd qualified as Ucd

data Ascii = Ascii !Word.Word64 !Word.Word64
  deriving (Eq, Show)

asciiHas :: Ascii -> Int -> Bool
asciiHas (Ascii low high) c =
  if c < 64
    then Bits.testBit low c
    else Bits.testBit high (c - 64)

asciiNone :: Ascii
asciiNone = Ascii 0 0

asciiAll :: Ascii
asciiAll = Ascii maxBound maxBound

asciiSingleton :: Int -> Ascii
asciiSingleton c =
  if c < 64
    then Ascii (Bits.bit c) 0
    else Ascii 0 (Bits.bit (c - 64))

asciiUnion :: Ascii -> Ascii -> Ascii
asciiUnion (Ascii a b) (Ascii c d) =
  Ascii (a Bits..|. c) (b Bits..|. d)

asciiIntersection :: Ascii -> Ascii -> Ascii
asciiIntersection (Ascii a b) (Ascii c d) =
  Ascii (a Bits..&. c) (b Bits..&. d)

asciiComplement :: Ascii -> Ascii
asciiComplement (Ascii a b) =
  Ascii (Bits.complement a) (Bits.complement b)

data Charset = Charset
  { charsetRanges :: !(PrimArray.PrimArray Int)
  , charsetAscii :: {-# UNPACK #-} !Ascii
  }

fromRanges :: [(Int, Int)] -> Charset
fromRanges =
  fromNormalized . Ranges.normalize

fromNormalized :: [(Int, Int)] -> Charset
fromNormalized ranges =
  Charset
    { charsetRanges = PrimArray.primArrayFromList (foldMap Bifoldable.biList ranges)
    , charsetAscii = asciiOfRanges ranges
    }

asciiOfRanges :: [(Int, Int)] -> Ascii
asciiOfRanges =
  let
    below k = if k >= 64 then maxBound else Bits.bit k - 1
    word base lo hi =
      let
        from = max 0 (lo - base)
        to = min 63 (hi - base)
       in
        if from > to then 0 else below (to + 1) Bits..&. Bits.complement (below from)
    add (Ascii low high) (lo, hi) =
      Ascii (low Bits..|. word 0 lo hi) (high Bits..|. word 64 lo hi)
   in
    Foldable.foldl' add asciiNone . takeWhile ((< 128) . fst)

singleton :: Int -> Charset
singleton c =
  fromNormalized [(c, c)]

toRanges :: Charset -> [(Int, Int)]
toRanges set =
  let
    ranges = charsetRanges set
   in
    fmap
      (\k -> (PrimArray.indexPrimArray ranges (2 * k), PrimArray.indexPrimArray ranges (2 * k + 1)))
      [0 .. rangeCount set - 1]

rangeCount :: Charset -> Int
rangeCount set =
  PrimArray.sizeofPrimArray (charsetRanges set) `div` 2

complement :: Charset -> Charset
complement set =
  Charset
    { charsetRanges = PrimArray.primArrayFromList (foldMap Bifoldable.biList (Ranges.complement (toRanges set)))
    , charsetAscii = asciiComplement (charsetAscii set)
    }

contains :: Charset -> Int -> Bool
contains set r =
  let
    ranges = charsetRanges set
    search lo hi
      | lo >= hi = False
      | otherwise =
          let
            mid = (lo + hi) `div` 2
           in
            if r > PrimArray.indexPrimArray ranges (2 * mid + 1)
              then search (mid + 1) hi
              else r >= PrimArray.indexPrimArray ranges (2 * mid) || search lo mid
   in
    if r < 128
      then r >= 0 && asciiHas (charsetAscii set) r
      else search 0 (rangeCount set)

isEmpty :: Charset -> Bool
isEmpty set =
  rangeCount set == 0

hasNonAscii :: Charset -> Bool
hasNonAscii set =
  let
    n = PrimArray.sizeofPrimArray (charsetRanges set)
   in
    n > 0 && PrimArray.indexPrimArray (charsetRanges set) (n - 1) >= 128

none :: Charset
none = fromNormalized []

dotSet :: Charset
dotSet = complement (fromRanges [(0x0A, 0x0A), (0x0D, 0x0D), (0x2028, 0x2029)])

anySet :: Charset
anySet = fromRanges [(0, Ranges.maxRune)]

digitSet :: Charset
digitSet = fromRanges digitRanges

digitRanges :: [(Int, Int)]
digitRanges = [(0x30, 0x39)]

notDigitSet :: Charset
notDigitSet = complement digitSet

wordSet :: Charset
wordSet = fromRanges wordRanges

wordRanges :: [(Int, Int)]
wordRanges = [(0x30, 0x39), (0x41, 0x5A), (0x5F, 0x5F), (0x61, 0x7A)]

notWordSet :: Charset
notWordSet = complement wordSet

spaceSet :: Charset
spaceSet = fromRanges spaceRanges

spaceRanges :: [(Int, Int)]
spaceRanges =
  [ (0x9, 0xD)
  , (0x20, 0x20)
  , (0xA0, 0xA0)
  , (0x1680, 0x1680)
  , (0x2000, 0x200A)
  , (0x2028, 0x2029)
  , (0x202F, 0x202F)
  , (0x205F, 0x205F)
  , (0x3000, 0x3000)
  , (0xFEFF, 0xFEFF)
  ]

notSpaceSet :: Charset
notSpaceSet = complement spaceSet

foldWordSet :: Charset
foldWordSet = fromRanges foldWordRanges

foldWordRanges :: [(Int, Int)]
foldWordRanges = wordRanges <> [(0x17F, 0x17F), (0x212A, 0x212A)]

notFoldWordSet :: Charset
notFoldWordSet = complement foldWordSet

isWordByte :: Int -> Bool
isWordByte b =
  b >= 0 && b < 128 && asciiHas (charsetAscii wordSet) b

isFoldWordRune :: Int -> Bool
isFoldWordRune =
  contains foldWordSet

propertyRanges :: Text.Text -> Maybe [(Int, Int)]
propertyRanges expr =
  case Text.breakOn "=" expr of
    (name, rest)
      | not (Text.null rest) ->
          let
            value = Text.drop 1 rest
           in
            case name of
              "General_Category" -> categoryRanges value
              "gc" -> categoryRanges value
              "Script" -> Ucd.script value
              "sc" -> Ucd.script value
              "Script_Extensions" -> Ucd.scriptExtensions value
              "scx" -> Ucd.scriptExtensions value
              _ -> Nothing
      | otherwise ->
          case categoryRanges expr of
            Just ranges -> Just ranges
            Nothing ->
              case Map.lookup expr binaryNames of
                Nothing -> Nothing
                Just "ASCII" -> Just [(0, 0x7F)]
                Just "Any" -> Just [(0, Ranges.maxRune)]
                Just canonical -> Ucd.binary canonical

categoryRanges :: Text.Text -> Maybe [(Int, Int)]
categoryRanges value =
  Ucd.category =<< Map.lookup value categoryNames

categoryNames :: Map.Map Text.Text Text.Text
categoryNames =
  let
    longNames =
      Map.fromList
        [ ("Letter", "L")
        , ("Lowercase_Letter", "Ll")
        , ("Uppercase_Letter", "Lu")
        , ("Titlecase_Letter", "Lt")
        , ("Cased_Letter", "LC")
        , ("Modifier_Letter", "Lm")
        , ("Other_Letter", "Lo")
        , ("Mark", "M")
        , ("Combining_Mark", "M")
        , ("Nonspacing_Mark", "Mn")
        , ("Spacing_Mark", "Mc")
        , ("Enclosing_Mark", "Me")
        , ("Number", "N")
        , ("Decimal_Number", "Nd")
        , ("digit", "Nd")
        , ("Letter_Number", "Nl")
        , ("Other_Number", "No")
        , ("Punctuation", "P")
        , ("punct", "P")
        , ("Connector_Punctuation", "Pc")
        , ("Dash_Punctuation", "Pd")
        , ("Open_Punctuation", "Ps")
        , ("Close_Punctuation", "Pe")
        , ("Initial_Punctuation", "Pi")
        , ("Final_Punctuation", "Pf")
        , ("Other_Punctuation", "Po")
        , ("Symbol", "S")
        , ("Math_Symbol", "Sm")
        , ("Currency_Symbol", "Sc")
        , ("Modifier_Symbol", "Sk")
        , ("Other_Symbol", "So")
        , ("Separator", "Z")
        , ("Space_Separator", "Zs")
        , ("Line_Separator", "Zl")
        , ("Paragraph_Separator", "Zp")
        , ("Other", "C")
        , ("Control", "Cc")
        , ("cntrl", "Cc")
        , ("Format", "Cf")
        , ("Surrogate", "Cs")
        , ("Private_Use", "Co")
        , ("Unassigned", "Cn")
        ]
   in
    longNames <> Map.fromList (fmap (\short -> (short, short)) (Map.elems longNames))

binaryNames :: Map.Map Text.Text Text.Text
binaryNames =
  Map.fromList
    . foldMap (\(canonical, aliases) -> fmap (\name -> (name, canonical)) (canonical : aliases))
    $ [ ("ASCII", [])
      , ("ASCII_Hex_Digit", ["AHex"])
      , ("Alphabetic", ["Alpha"])
      , ("Any", [])
      , ("Assigned", [])
      , ("Bidi_Control", ["Bidi_C"])
      , ("Bidi_Mirrored", ["Bidi_M"])
      , ("Case_Ignorable", ["CI"])
      , ("Cased", [])
      , ("Changes_When_Casefolded", ["CWCF"])
      , ("Changes_When_Casemapped", ["CWCM"])
      , ("Changes_When_Lowercased", ["CWL"])
      , ("Changes_When_NFKC_Casefolded", ["CWKCF"])
      , ("Changes_When_Titlecased", ["CWT"])
      , ("Changes_When_Uppercased", ["CWU"])
      , ("Dash", [])
      , ("Default_Ignorable_Code_Point", ["DI"])
      , ("Deprecated", ["Dep"])
      , ("Diacritic", ["Dia"])
      , ("Emoji", [])
      , ("Emoji_Component", ["EComp"])
      , ("Emoji_Modifier", ["EMod"])
      , ("Emoji_Modifier_Base", ["EBase"])
      , ("Emoji_Presentation", ["EPres"])
      , ("Extended_Pictographic", ["ExtPict"])
      , ("Extender", ["Ext"])
      , ("Grapheme_Base", ["Gr_Base"])
      , ("Grapheme_Extend", ["Gr_Ext"])
      , ("Hex_Digit", ["Hex"])
      , ("IDS_Binary_Operator", ["IDSB"])
      , ("IDS_Trinary_Operator", ["IDST"])
      , ("ID_Continue", ["IDC"])
      , ("ID_Start", ["IDS"])
      , ("Ideographic", ["Ideo"])
      , ("Join_Control", ["Join_C"])
      , ("Logical_Order_Exception", ["LOE"])
      , ("Lowercase", ["Lower"])
      , ("Math", [])
      , ("Noncharacter_Code_Point", ["NChar"])
      , ("Pattern_Syntax", ["Pat_Syn"])
      , ("Pattern_White_Space", ["Pat_WS"])
      , ("Quotation_Mark", ["QMark"])
      , ("Radical", [])
      , ("Regional_Indicator", ["RI"])
      , ("Sentence_Terminal", ["STerm"])
      , ("Soft_Dotted", ["SD"])
      , ("Terminal_Punctuation", ["Term"])
      , ("Unified_Ideograph", ["UIdeo"])
      , ("Uppercase", ["Upper"])
      , ("Variation_Selector", ["VS"])
      , ("White_Space", ["space"])
      , ("XID_Continue", ["XIDC"])
      , ("XID_Start", ["XIDS"])
      ]

canonicalize :: Mode.Mode -> Int -> Int
canonicalize mode r =
  case mode of
    Mode.UnicodeMode -> Ucd.fold r
    Mode.LegacyMode ->
      if r > 0xFFFF || contains multiUpper r
        then r
        else
          let
            upper = Ucd.toUpper r
           in
            if (r >= 128 && upper < 128) || upper > 0xFFFF then r else upper

multiUpper :: Charset
multiUpper =
  fromRanges
    [ (0xDF, 0xDF)
    , (0x149, 0x149)
    , (0x1F0, 0x1F0)
    , (0x390, 0x390)
    , (0x3B0, 0x3B0)
    , (0x587, 0x587)
    , (0x1E96, 0x1E9A)
    , (0x1F50, 0x1F50)
    , (0x1F52, 0x1F52)
    , (0x1F54, 0x1F54)
    , (0x1F56, 0x1F56)
    , (0x1F80, 0x1FAF)
    , (0x1FB2, 0x1FB4)
    , (0x1FB6, 0x1FB7)
    , (0x1FBC, 0x1FBC)
    , (0x1FC2, 0x1FC4)
    , (0x1FC6, 0x1FC7)
    , (0x1FCC, 0x1FCC)
    , (0x1FD2, 0x1FD3)
    , (0x1FD6, 0x1FD7)
    , (0x1FE2, 0x1FE4)
    , (0x1FE6, 0x1FE7)
    , (0x1FF2, 0x1FF4)
    , (0x1FF6, 0x1FF7)
    , (0x1FFC, 0x1FFC)
    , (0xFB00, 0xFB06)
    , (0xFB13, 0xFB17)
    ]

foldClosureCounted :: Mode.Mode -> Charset -> (Charset, Int)
foldClosureCounted mode set =
  let
    table = foldTable mode
    within (lo, hi) =
      fst . IntMap.split (hi + 1) . snd . IntMap.split (lo - 1) $ table
    found = fmap within (toRanges set)
    variants = sum (fmap IntMap.size found)
    extra =
      filter (not . contains set)
        . foldMap (foldMap snd . IntMap.toList)
        $ found
   in
    if null extra
      then (set, variants)
      else (fromRanges (fmap (\c -> (c, c)) extra <> toRanges set), variants)

foldTable :: Mode.Mode -> FoldTable
foldTable mode =
  case mode of
    Mode.UnicodeMode -> unicodeFoldTable
    Mode.LegacyMode -> legacyFoldTable

type FoldTable = IntMap.IntMap [Int]

unicodeFoldTable :: FoldTable
unicodeFoldTable =
  let
    keys = fmap Ucd.fold Ucd.folding
   in
    foldTableOf (fmap (\key -> (key, key)) keys <> fmap (\r -> (Ucd.fold r, r)) Ucd.folding)

foldTableOf :: [(Int, Int)] -> FoldTable
foldTableOf keyed =
  let
    classes =
      fmap IntSet.toList
        . IntMap.elems
        . IntMap.fromListWith IntSet.union
        . fmap (Bifunctor.second IntSet.singleton)
        $ keyed
    multiple members = case members of
      _ : _ : _ -> True
      _ -> False
   in
    IntMap.fromList
      . foldMap (\members -> fmap (\member -> (member, members)) members)
      . filter multiple
      $ classes

legacyFoldTable :: FoldTable
legacyFoldTable =
  foldTableOf (fmap (\r -> (canonicalize Mode.LegacyMode r, r)) [0 .. 0xFFFF])

foldClosure :: Mode.Mode -> Charset -> Charset
foldClosure mode =
  fst . foldClosureCounted mode
