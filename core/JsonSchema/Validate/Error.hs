module JsonSchema.Validate.Error
  ( ValidationError (..)
  , validationError
  , validationErrorWithContext
  , validationErrorAtPath
  , errorKeyword
  , errorMessage
  , renderValidationError
  , renderErrors
  , renderJsonPointer
  , ValidationErrorKind (..)
  , keywordOfKind
  , renderErrorKind
  ) where

import Control.DeepSeq qualified as DeepSeq
import Data.Aeson qualified as Aeson
import Data.Aeson.Text qualified as AesonText
import Data.Foldable qualified as Foldable
import Data.List.NonEmpty qualified as NEL
import Data.Scientific qualified as Scientific
import Data.Text qualified as Text
import Data.Text.Lazy qualified as TextLazy
import GHC.Generics qualified as Generics

import JsonSchema qualified as Schema
import JsonSchema.Validate.Path qualified as Path

data ValidationError = ValidationError
  { errorInstancePath :: ![Text.Text]
  , errorSchemaPath :: ![Text.Text]
  , errorKind :: !ValidationErrorKind
  , errorContext :: ![ValidationError]
  -- ^ The subschema errors behind an @anyOf@ failure, or a @oneOf@ failure with no matches. Empty for every other error.
  }
  deriving stock (Eq, Generics.Generic, Show)
  deriving anyclass (DeepSeq.NFData)

data ValidationErrorKind
  = FalseSchemaRejection
  | TypeMismatch !(NEL.NonEmpty Schema.SchemaType) !Aeson.Value
  | EnumNotMatched ![Aeson.Value] !Aeson.Value
  | ConstMismatch !Aeson.Value !Aeson.Value
  | MinimumFailed !Scientific.Scientific !Scientific.Scientific
  | MaximumFailed !Scientific.Scientific !Scientific.Scientific
  | ExclusiveMinimumFailed !Scientific.Scientific !Scientific.Scientific
  | ExclusiveMaximumFailed !Scientific.Scientific !Scientific.Scientific
  | MultipleOfFailed !Scientific.Scientific !Scientific.Scientific
  | MinLengthFailed !Word !Int
  | MaxLengthFailed !Word !Int
  | PatternFailed !Text.Text !Text.Text
  | PatternCompileFailed !Text.Text !Text.Text
  | FormatFailed !Text.Text !Text.Text
  | MinItemsFailed !Word !Int
  | MaxItemsFailed !Word !Int
  | UniqueItemsFailed
  | ContainsFailed
  | ContainsTooFew !Word !Int
  | ContainsTooMany !Word !Int
  | MinPropertiesFailed !Word !Int
  | MaxPropertiesFailed !Word !Int
  | RequiredMissing !Text.Text
  | DependentRequiredMissing !Text.Text !Text.Text !Text.Text
  | AnyOfFailed
  | OneOfFailed !Int
  | NotMatched
  | RefUnresolved !Text.Text !Text.Text
  | RefCycle !Text.Text !Text.Text
  deriving stock (Eq, Generics.Generic, Show)
  deriving anyclass (DeepSeq.NFData)

validationError :: Path.InstancePath -> Path.SchemaPath -> ValidationErrorKind -> ValidationError
validationError ip sp kind = validationErrorWithContext ip sp kind []

validationErrorWithContext ::
  Path.InstancePath -> Path.SchemaPath -> ValidationErrorKind -> [ValidationError] -> ValidationError
validationErrorWithContext ip sp kind context =
  (validationErrorAtPath ip (sp <> foldMap Path.schemaSegment (keywordOfKind kind)) kind){errorContext = context}

validationErrorAtPath :: Path.InstancePath -> Path.SchemaPath -> ValidationErrorKind -> ValidationError
validationErrorAtPath ip sp kind =
  ValidationError
    { errorInstancePath = Path.instancePathToList ip
    , errorSchemaPath = Path.schemaPathToList sp
    , errorKind = kind
    , errorContext = []
    }

keywordOfKind :: ValidationErrorKind -> Maybe Text.Text
keywordOfKind f = case f of
  FalseSchemaRejection -> Nothing
  TypeMismatch{} -> Just "type"
  EnumNotMatched{} -> Just "enum"
  ConstMismatch{} -> Just "const"
  MinimumFailed{} -> Just "minimum"
  MaximumFailed{} -> Just "maximum"
  ExclusiveMinimumFailed{} -> Just "exclusiveMinimum"
  ExclusiveMaximumFailed{} -> Just "exclusiveMaximum"
  MultipleOfFailed{} -> Just "multipleOf"
  MinLengthFailed{} -> Just "minLength"
  MaxLengthFailed{} -> Just "maxLength"
  PatternFailed{} -> Just "pattern"
  PatternCompileFailed keyword _ -> Just keyword
  FormatFailed{} -> Just "format"
  MinItemsFailed{} -> Just "minItems"
  MaxItemsFailed{} -> Just "maxItems"
  UniqueItemsFailed -> Just "uniqueItems"
  ContainsFailed -> Just "contains"
  ContainsTooFew{} -> Just "minContains"
  ContainsTooMany{} -> Just "maxContains"
  MinPropertiesFailed{} -> Just "minProperties"
  MaxPropertiesFailed{} -> Just "maxProperties"
  RequiredMissing{} -> Just "required"
  DependentRequiredMissing keyword _ _ -> Just keyword
  AnyOfFailed -> Just "anyOf"
  OneOfFailed{} -> Just "oneOf"
  NotMatched -> Just "not"
  RefUnresolved keyword _ -> Just keyword
  RefCycle keyword _ -> Just keyword

errorKeyword :: ValidationError -> Maybe Text.Text
errorKeyword = keywordOfKind . errorKind

errorMessage :: ValidationError -> Text.Text
errorMessage = renderErrorKind . errorKind

renderErrorKind :: ValidationErrorKind -> Text.Text
renderErrorKind f = case f of
  FalseSchemaRejection ->
    "value rejected by false schema"
  TypeMismatch types _ ->
    "value does not match any of the expected types: "
      <> Text.intercalate ", " (fmap Schema.schemaTypeToText . Foldable.toList $ types)
  EnumNotMatched _ actual -> "value not in enum: " <> renderAesonValue actual
  ConstMismatch expected actual ->
    "value " <> renderAesonValue actual <> " does not match const " <> renderAesonValue expected
  MinimumFailed bound _ -> "value below minimum " <> renderScientific bound
  MaximumFailed bound _ -> "value above maximum " <> renderScientific bound
  ExclusiveMinimumFailed bound _ -> "value at or below exclusive minimum " <> renderScientific bound
  ExclusiveMaximumFailed bound _ -> "value at or above exclusive maximum " <> renderScientific bound
  MultipleOfFailed factor _ -> "value is not a multiple of " <> renderScientific factor
  MinLengthFailed bound _ -> "string shorter than minLength " <> renderWord bound
  MaxLengthFailed bound _ -> "string longer than maxLength " <> renderWord bound
  PatternFailed pat _ -> "string does not match pattern " <> pat
  PatternCompileFailed _ pat -> "pattern is not a valid regex: " <> pat
  FormatFailed fmt _ -> "string does not match format: " <> fmt
  MinItemsFailed bound _ -> "array has fewer items than minItems " <> renderWord bound
  MaxItemsFailed bound _ -> "array has more items than maxItems " <> renderWord bound
  UniqueItemsFailed -> "array items are not unique"
  ContainsFailed -> "no array item matches contains schema"
  ContainsTooFew bound _ -> "array has fewer matches than minContains " <> renderWord bound
  ContainsTooMany bound _ -> "array has more matches than maxContains " <> renderWord bound
  MinPropertiesFailed bound _ -> "object has fewer properties than minProperties " <> renderWord bound
  MaxPropertiesFailed bound _ -> "object has more properties than maxProperties " <> renderWord bound
  RequiredMissing k -> "missing required property: " <> k
  DependentRequiredMissing _ depKey k ->
    "missing dependency " <> k <> " (required by " <> depKey <> ")"
  AnyOfFailed -> "value does not match any schema in anyOf"
  OneOfFailed matched ->
    "value does not match exactly one schema in oneOf (matched " <> renderInt matched <> ")"
  NotMatched -> "value should not match the not schema"
  RefUnresolved keyword ref -> "could not resolve " <> keyword <> ": " <> ref
  RefCycle keyword ref -> keyword <> " cycle returns to the same instance location: " <> ref

renderAesonValue :: Aeson.Value -> Text.Text
renderAesonValue = TextLazy.toStrict . AesonText.encodeToLazyText

renderScientific :: Scientific.Scientific -> Text.Text
renderScientific = Text.pack . show

renderWord :: Word -> Text.Text
renderWord = Text.pack . show

renderInt :: Int -> Text.Text
renderInt = Text.pack . show

renderValidationError :: ValidationError -> Text.Text
renderValidationError err =
  let
    locator =
      if null (errorInstancePath err)
        then "(root)"
        else renderJsonPointer (errorInstancePath err)
    schemaLoc = "[" <> renderJsonPointer (errorSchemaPath err) <> "]"
   in
    locator <> " " <> schemaLoc <> " " <> errorMessage err

renderJsonPointer :: [Text.Text] -> Text.Text
renderJsonPointer = foldMap (("/" <>) . Text.replace "/" "~1" . Text.replace "~" "~0")

-- | One error per line, with no trailing newline.
renderErrors :: [ValidationError] -> Text.Text
renderErrors = Text.intercalate "\n" . fmap renderValidationError
