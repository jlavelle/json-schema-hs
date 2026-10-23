module JsonSchema.Validate.String
  ( compilePattern
  ) where

import Data.Text qualified as Text
import Text.Regex.TDFA qualified as Regex

compilePattern :: Text.Text -> Maybe (Text.Text -> Bool)
compilePattern pat =
  fmap
    (Regex.match :: Regex.Regex -> Text.Text -> Bool)
    (Regex.makeRegexOptsM patternCompOpt Regex.defaultExecOpt pat)

patternCompOpt :: Regex.CompOption
patternCompOpt = Regex.defaultCompOpt{Regex.multiline = False}
