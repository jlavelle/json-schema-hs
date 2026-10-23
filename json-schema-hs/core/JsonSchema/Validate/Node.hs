module JsonSchema.Validate.Node
  ( NodeValidator (NodeValidator, runNodeValidator)
  , CompiledPattern (..)
  , compiledPatternMatcher
  , mkCompiledPattern
  , CompiledObj (..)
  , Validator (..)
  ) where

import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Text qualified as Text

import JsonSchema.Validate.Dialect qualified as Dialect
import JsonSchema.Validate.Evaluation qualified as Evaluation
import JsonSchema.Validate.Format qualified as Format
import JsonSchema.Validate.Numeric qualified as Numeric
import JsonSchema.Validate.Path qualified as Path
import JsonSchema.Validate.Registry qualified as Registry
import JsonSchema.Validate.String qualified as VString

newtype NodeValidator = NodeValidator
  { runNodeValidator ::
      Path.InstancePath ->
      Path.SchemaPath ->
      Registry.DynamicScope ->
      Aeson.Value ->
      Evaluation.Evaluation
  }

data CompiledPattern
  = Compiled !Text.Text !(Text.Text -> Bool)
  | Uncompilable !Text.Text

compiledPatternMatcher :: CompiledPattern -> Maybe (Text.Text -> Bool)
compiledPatternMatcher cp = case cp of
  Compiled _ matches -> Just matches
  Uncompilable _ -> Nothing

mkCompiledPattern :: Text.Text -> CompiledPattern
mkCompiledPattern pat = case VString.compilePattern pat of
  Just matches -> Compiled pat matches
  Nothing -> Uncompilable pat

data CompiledObj = CompiledObj
  { compiledStringPattern :: !(Maybe CompiledPattern)
  , compiledPatternProperties :: !(Maybe [(CompiledPattern, NodeValidator)])
  , compiledAdditionalPatternMatchers :: ![Text.Text -> Bool]
  , compiledPropValidators :: !(KeyMap.KeyMap NodeValidator)
  , compiledPropValidatorCount :: !Int
  , compiledFormat :: !(Maybe (Text.Text, Format.FormatPredicate))
  , compiledMultipleOf :: !(Maybe Numeric.NormalizedScientific)
  , compiledPrefixItems :: ![NodeValidator]
  , compiledPrefixItemsLength :: !Int
  , compiledVocabularyFlags :: !Dialect.VocabularyFlags
  }

newtype Validator = Validator
  { validatorRoot :: NodeValidator
  }
