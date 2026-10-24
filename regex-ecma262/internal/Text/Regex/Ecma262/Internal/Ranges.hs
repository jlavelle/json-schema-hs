module Text.Regex.Ecma262.Internal.Ranges
  ( maxRune
  , normalize
  , complement
  ) where

import Data.List qualified as List

maxRune :: Int
maxRune = 0x10FFFF

normalize :: [(Int, Int)] -> [(Int, Int)]
normalize =
  let
    merge sorted =
      case sorted of
        (lo, hi) : (nextLo, nextHi) : rest
          | nextLo <= hi + 1 -> merge ((lo, max hi nextHi) : rest)
          | otherwise -> (lo, hi) : merge ((nextLo, nextHi) : rest)
        _ -> sorted
   in
    merge . List.sort

complement :: [(Int, Int)] -> [(Int, Int)]
complement =
  let
    go next ranges =
      case ranges of
        [] -> filter ((<= maxRune) . fst) [(next, maxRune)]
        (lo, hi) : rest
          | lo > next -> (next, lo - 1) : go (hi + 1) rest
          | otherwise -> go (hi + 1) rest
   in
    go 0
