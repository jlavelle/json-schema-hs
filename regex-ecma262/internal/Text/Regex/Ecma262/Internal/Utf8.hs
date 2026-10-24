module Text.Regex.Ecma262.Internal.Utf8
  ( Bytes
  , byte
  , rune
  , width
  , packed
  , runeError
  , runeSelf
  , decode
  , decodeLast
  , unitAt
  , unitBefore
  , highSurrogate
  , lowSurrogate
  , validRune
  , runeLength
  , encodeRune
  , combineSurrogates
  ) where

import Data.Bits qualified as Bits
import Data.Text.Array qualified as TextArray

type Bytes = TextArray.Array

byte :: Bytes -> Int -> Int
byte bytes i =
  fromIntegral (TextArray.unsafeIndex bytes i)

rune :: Int -> Int
rune p =
  Bits.shiftR p 3

width :: Int -> Int
width p =
  p Bits..&. 7

packed :: Int -> Int -> Int
packed r w =
  Bits.shiftL r 3 Bits..|. w

runeError :: Int
runeError = 0xFFFD

runeSelf :: Int
runeSelf = 0x80

decode :: Bytes -> Int -> Int -> Int
decode t at limit =
  let
    p0 = byte t at
    n = limit - at
    continuation b = b >= 0x80 && b <= 0xBF
   in
    if p0 < 0x80
      then packed p0 1
      else
        if p0 < 0xC2 || p0 > 0xF4
          then error1
          else
            if p0 < 0xE0
              then
                if n < 2
                  then error1
                  else
                    let
                      b1 = byte t (at + 1)
                     in
                      if continuation b1
                        then packed (Bits.shiftL (p0 Bits..&. 0x1F) 6 Bits..|. (b1 Bits..&. 0x3F)) 2
                        else error1
              else
                if p0 < 0xF0
                  then
                    if n < 3
                      then error1
                      else
                        let
                          b1 = byte t (at + 1)
                          b2 = byte t (at + 2)
                          b1Low = if p0 == 0xE0 then 0xA0 else 0x80
                          b1High = if p0 == 0xED then 0x9F else 0xBF
                         in
                          if b1 < b1Low || b1 > b1High || not (continuation b2)
                            then error1
                            else
                              packed
                                ( Bits.shiftL (p0 Bits..&. 0x0F) 12
                                    Bits..|. Bits.shiftL (b1 Bits..&. 0x3F) 6
                                    Bits..|. (b2 Bits..&. 0x3F)
                                )
                                3
                  else
                    if n < 4
                      then error1
                      else
                        let
                          b1 = byte t (at + 1)
                          b2 = byte t (at + 2)
                          b3 = byte t (at + 3)
                          b1Low = if p0 == 0xF0 then 0x90 else 0x80
                          b1High = if p0 == 0xF4 then 0x8F else 0xBF
                         in
                          if b1 < b1Low || b1 > b1High || not (continuation b2) || not (continuation b3)
                            then error1
                            else
                              packed
                                ( Bits.shiftL (p0 Bits..&. 0x07) 18
                                    Bits..|. Bits.shiftL (b1 Bits..&. 0x3F) 12
                                    Bits..|. Bits.shiftL (b2 Bits..&. 0x3F) 6
                                    Bits..|. (b3 Bits..&. 0x3F)
                                )
                                4

error1 :: Int
error1 = packed runeError 1

decodeLast :: Bytes -> Int -> Int -> Int
decodeLast t lo limit =
  let
    r = byte t (limit - 1)
    lim = max (limit - 4) lo
    runeStart start
      | start < lim = lim
      | byte t start Bits..&. 0xC0 /= 0x80 = start
      | otherwise = runeStart (start - 1)
   in
    if r < 0x80
      then packed r 1
      else
        let
          start = runeStart (limit - 2)
          p = decode t start limit
         in
          if start + width p /= limit then error1 else p

unitAt :: Bytes -> Int -> Int -> Int -> Int
unitAt t lo at hi =
  let
    c = byte t at
   in
    if c >= 0xF0
      then
        let
          p = decode t at hi
         in
          if width p == 4 then packed (highSurrogate (rune p)) 2 else error1
      else
        if c >= 0x80 && c < 0xC0 && at - lo >= 2 && byte t (at - 2) >= 0xF0
          then
            let
              p = decode t (at - 2) hi
             in
              if width p == 4 then packed (lowSurrogate (rune p)) 2 else decode t at hi
          else decode t at hi

highSurrogate :: Int -> Int
highSurrogate r =
  0xD800 + Bits.shiftR (r - 0x10000) 10

lowSurrogate :: Int -> Int
lowSurrogate r =
  0xDC00 + ((r - 0x10000) Bits..&. 0x3FF)

unitBefore :: Bytes -> Int -> Int -> Int -> Int
unitBefore t lo at hi =
  let
    p = decodeLast t lo at
   in
    if width p == 4
      then packed (lowSurrogate (rune p)) 2
      else
        if p == error1 && at - lo >= 2 && hi - at >= 2 && byte t (at - 2) >= 0xF0
          then
            let
              whole = decode t (at - 2) hi
             in
              if width whole == 4 then packed (highSurrogate (rune whole)) 2 else p
          else p

validRune :: Int -> Bool
validRune r =
  (r >= 0 && r < 0xD800) || (r >= 0xE000 && r <= 0x10FFFF)

runeLength :: Int -> Int
runeLength r
  | r < 0x80 = 1
  | r < 0x800 = 2
  | r < 0x10000 = 3
  | otherwise = 4

encodeRune :: Int -> [Int]
encodeRune code =
  let
    r = if validRune code then code else runeError
    continuation shift = 0x80 Bits..|. (Bits.shiftR r shift Bits..&. 0x3F)
   in
    if r < 0x80
      then [r]
      else
        if r < 0x800
          then [0xC0 Bits..|. Bits.shiftR r 6, continuation 0]
          else
            if r < 0x10000
              then [0xE0 Bits..|. Bits.shiftR r 12, continuation 6, continuation 0]
              else [0xF0 Bits..|. Bits.shiftR r 18, continuation 12, continuation 6, continuation 0]

combineSurrogates :: Int -> Int -> Int
combineSurrogates high low =
  0x10000 + Bits.shiftL (high - 0xD800) 10 + (low - 0xDC00)
