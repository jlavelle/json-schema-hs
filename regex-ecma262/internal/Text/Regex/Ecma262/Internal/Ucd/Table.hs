{-# LANGUAGE MagicHash #-}

module Text.Regex.Ecma262.Internal.Ucd.Table
  ( Table (..)
  , CaseTable (..)
  , tableContains
  , tableRanges
  , caseTableMap
  , caseTableChanged
  ) where

import Data.Bits qualified as Bits
import GHC.Exts qualified as Exts

data Table = Table
  { tableR16 :: Exts.Addr#
  , tableR16Length :: !Int
  , tableR32 :: Exts.Addr#
  , tableR32Length :: !Int
  }

data CaseTable = CaseTable
  { caseTableRows :: Exts.Addr#
  , caseTableRowCount :: !Int
  }

tableContains :: Table -> Int -> Bool
tableContains t r =
  let
    search bound lo hi
      | lo >= hi = False
      | otherwise =
          let
            mid = (lo + hi) `div` 2
           in
            if r > bound (2 * mid + 1)
              then search bound (mid + 1) hi
              else r >= bound (2 * mid) || search bound lo mid
   in
    r >= 0
      && if r <= 0xFFFF
        then search (bound16 t) 0 (tableR16Length t `div` 4)
        else search (bound24 t) 0 (tableR32Length t `div` 6)

bound16 :: Table -> Int -> Int
bound16 t i =
  byteAt (tableR16 t) (2 * i) Bits..|. Bits.shiftL (byteAt (tableR16 t) (2 * i + 1)) 8

byteAt :: Exts.Addr# -> Int -> Int
byteAt addr (Exts.I# i) =
  Exts.I# (Exts.word2Int# (Exts.word8ToWord# (Exts.indexWord8OffAddr# addr i)))

bound24 :: Table -> Int -> Int
bound24 t i =
  byteAt (tableR32 t) (3 * i)
    Bits..|. Bits.shiftL (byteAt (tableR32 t) (3 * i + 1)) 8
    Bits..|. Bits.shiftL (byteAt (tableR32 t) (3 * i + 2)) 16

tableRanges :: Table -> [(Int, Int)]
tableRanges t =
  let
    n16 = tableR16Length t `div` 4
    n32 = tableR32Length t `div` 6
    lows = fmap (\i -> (bound16 t (2 * i), bound16 t (2 * i + 1))) [0 .. n16 - 1]
    highs = fmap (\i -> (bound24 t (2 * i), bound24 t (2 * i + 1))) [0 .. n32 - 1]
   in
    joinAtPlane lows highs

joinAtPlane :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
joinAtPlane lows highs =
  case (reverse lows, highs) of
    ((lo, 0xFFFF) : earlier, (0x10000, hi) : later) -> reverse earlier <> ((lo, hi) : later)
    _ -> lows <> highs

caseTableMap :: CaseTable -> Int -> Int
caseTableMap table r =
  let
    field row k = int32At (caseTableRows table) (4 * row + k)
    go lo hi
      | lo >= hi = r
      | otherwise =
          let
            mid = (lo + hi) `div` 2
            first = field mid 0
            offset = r - first
           in
            if r < first
              then go lo mid
              else
                if offset >= field mid 2
                  then go (mid + 1) hi
                  else
                    if offset Bits..&. field mid 3 == 0
                      then r + field mid 1
                      else r
   in
    go 0 (caseTableRowCount table)

int32At :: Exts.Addr# -> Int -> Int
int32At addr i =
  let
    unsigned =
      byteAt addr (4 * i)
        Bits..|. Bits.shiftL (byteAt addr (4 * i + 1)) 8
        Bits..|. Bits.shiftL (byteAt addr (4 * i + 2)) 16
        Bits..|. Bits.shiftL (byteAt addr (4 * i + 3)) 24
   in
    if unsigned >= 0x80000000 then unsigned - 0x100000000 else unsigned

caseTableChanged :: CaseTable -> [Int]
caseTableChanged table =
  let
    field row k = int32At (caseTableRows table) (4 * row + k)
    rowPoints row =
      if field row 1 == 0
        then []
        else
          fmap (field row 0 +)
            . filter (\offset -> offset Bits..&. field row 3 == 0)
            $ [0 .. field row 2 - 1]
   in
    foldMap rowPoints [0 .. caseTableRowCount table - 1]
