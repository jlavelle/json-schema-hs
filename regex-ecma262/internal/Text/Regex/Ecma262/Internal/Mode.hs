module Text.Regex.Ecma262.Internal.Mode
  ( Mode (..)
  ) where

data Mode
  = UnicodeMode
  | LegacyMode
  deriving (Eq, Show)
