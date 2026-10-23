module JsonSchema.Validate.Numeric
  ( NormalizedScientific
  , normalizedSource
  , normalizeScientific
  , isMultipleOfNormalized
  ) where

import Data.Scientific qualified as Scientific

data NormalizedScientific = NormalizedScientific
  { normalizedSource :: !Scientific.Scientific
  , normalizedCoefficient :: !Integer
  , normalizedExponent :: !Int
  }

normalizeScientific :: Scientific.Scientific -> Maybe NormalizedScientific
normalizeScientific s
  | s == 0 = Nothing
  | otherwise =
      let
        n = Scientific.normalize s
       in
        Just
          NormalizedScientific
            { normalizedSource = s
            , normalizedCoefficient = Scientific.coefficient n
            , normalizedExponent = Scientific.base10Exponent n
            }

isMultipleOfNormalized :: Scientific.Scientific -> NormalizedScientific -> Bool
isMultipleOfNormalized n d =
  let
    mCoeff = normalizedCoefficient d
    mExp = normalizedExponent d
    nNormalized = Scientific.normalize n
    e1 = Scientific.base10Exponent nNormalized
    c1 = Scientific.coefficient nNormalized
   in
    if e1 <= mExp
      then c1 `mod` (mCoeff * (10 ^ (mExp - e1))) == 0
      else (c1 * (10 ^ (e1 - mExp))) `mod` mCoeff == 0
