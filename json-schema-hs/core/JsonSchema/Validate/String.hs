module JsonSchema.Validate.String
  ( compilePattern
  ) where

import Data.Text qualified as Text
import Text.Regex.Ecma262 qualified as Ecma262

compilePattern :: Text.Text -> Maybe (Text.Text -> Bool)
compilePattern pat =
  case Ecma262.compile pat of
    Right regex -> Just (Ecma262.isMatch regex)
    Left _ -> Nothing
