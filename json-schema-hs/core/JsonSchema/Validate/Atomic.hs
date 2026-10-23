module JsonSchema.Validate.Atomic
  ( validateType
  , validateEnum
  , validateConst
  , validateNumeric
  , validateString
  , validateFormat
  ) where

import Data.Aeson qualified as Aeson
import Data.Foldable qualified as Foldable
import Data.Scientific qualified as Scientific
import Data.Text qualified as Text

import JsonSchema qualified as Schema
import JsonSchema.Validate.Error qualified as Error
import JsonSchema.Validate.Evaluation qualified as Evaluation
import JsonSchema.Validate.Format qualified as Format
import JsonSchema.Validate.Node qualified as Node
import JsonSchema.Validate.Numeric qualified as Numeric
import JsonSchema.Validate.Path qualified as Path

validateType ::
  Path.InstancePath ->
  Path.SchemaPath ->
  Schema.SchemaObjectF r ->
  Aeson.Value ->
  Evaluation.Evaluation
validateType ip sp obj value =
  foldMap
    ( \types ->
        if any (matchesType value) types
          then Evaluation.emptyResult
          else Evaluation.errorResult (Error.validationError ip sp (Error.TypeMismatch types value))
    )
    (Schema.schemaType obj)

matchesType :: Aeson.Value -> Schema.SchemaType -> Bool
matchesType value ty = case (value, ty) of
  (Aeson.Null, Schema.TypeNull) -> True
  (Aeson.Bool _, Schema.TypeBoolean) -> True
  (Aeson.Object _, Schema.TypeObject) -> True
  (Aeson.Array _, Schema.TypeArray) -> True
  (Aeson.Number _, Schema.TypeNumber) -> True
  (Aeson.Number n, Schema.TypeInteger) -> Scientific.isInteger n
  (Aeson.String _, Schema.TypeString) -> True
  _ -> False

validateEnum ::
  Path.InstancePath ->
  Path.SchemaPath ->
  Schema.SchemaObjectF r ->
  Aeson.Value ->
  Evaluation.Evaluation
validateEnum ip sp obj value =
  foldMap
    ( \values ->
        if value `elem` values
          then Evaluation.emptyResult
          else Evaluation.errorResult (Error.validationError ip sp (Error.EnumNotMatched values value))
    )
    (Schema.schemaEnum obj)

validateConst ::
  Path.InstancePath ->
  Path.SchemaPath ->
  Schema.SchemaObjectF r ->
  Aeson.Value ->
  Evaluation.Evaluation
validateConst ip sp obj value =
  foldMap
    ( \expected ->
        if expected == value
          then Evaluation.emptyResult
          else Evaluation.errorResult (Error.validationError ip sp (Error.ConstMismatch expected value))
    )
    (Schema.schemaConst obj)

validateNumeric ::
  Path.InstancePath ->
  Path.SchemaPath ->
  Schema.SchemaObjectF r ->
  Maybe Numeric.NormalizedScientific ->
  Aeson.Value ->
  Evaluation.Evaluation
validateNumeric ip sp obj compiledMultiple value = case value of
  Aeson.Number n ->
    Foldable.fold
      [ Evaluation.checkBound ip sp (n >=) (`Error.MinimumFailed` n) (Schema.schemaMinimum obj)
      , Evaluation.checkBound ip sp (n <=) (`Error.MaximumFailed` n) (Schema.schemaMaximum obj)
      , Evaluation.checkBound ip sp (n >) (`Error.ExclusiveMinimumFailed` n) (Schema.schemaExclusiveMinimum obj)
      , Evaluation.checkBound ip sp (n <) (`Error.ExclusiveMaximumFailed` n) (Schema.schemaExclusiveMaximum obj)
      , Evaluation.checkBound
          ip
          sp
          (Numeric.isMultipleOfNormalized n)
          (\nm -> Error.MultipleOfFailed (Numeric.normalizedSource nm) n)
          compiledMultiple
      ]
  _ -> Evaluation.emptyResult

validateString ::
  Path.InstancePath ->
  Path.SchemaPath ->
  Schema.SchemaObjectF r ->
  Maybe Node.CompiledPattern ->
  Aeson.Value ->
  Evaluation.Evaluation
validateString ip sp obj compiledStringPattern value = case value of
  Aeson.String s ->
    let
      len = Text.length s
     in
      Foldable.fold
        [ Evaluation.checkBound ip sp (fromIntegral len >=) (`Error.MinLengthFailed` len) (Schema.schemaMinLength obj)
        , Evaluation.checkBound ip sp (fromIntegral len <=) (`Error.MaxLengthFailed` len) (Schema.schemaMaxLength obj)
        , foldMap
            ( \compiled -> case compiled of
                Node.Uncompilable src ->
                  Evaluation.errorResult (Error.validationError ip sp (Error.PatternCompileFailed "pattern" src))
                Node.Compiled src matches ->
                  if matches s
                    then Evaluation.emptyResult
                    else Evaluation.errorResult (Error.validationError ip sp (Error.PatternFailed src s))
            )
            compiledStringPattern
        ]
  _ -> Evaluation.emptyResult

validateFormat ::
  Path.InstancePath ->
  Path.SchemaPath ->
  Maybe (Text.Text, Format.FormatPredicate) ->
  Aeson.Value ->
  Evaluation.Evaluation
validateFormat ip sp compiledFormat value = case (compiledFormat, value) of
  (Just (fmt, predicate), Aeson.String s)
    | not (Format.runFormatPredicate predicate s) ->
        Evaluation.errorResult (Error.validationError ip sp (Error.FormatFailed fmt s))
  _ -> Evaluation.emptyResult
