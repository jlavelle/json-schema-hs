{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Example
  ( Task (..)
  , taskSchema
  , taskJsonSchema
  , taskValidator
  , checkTask
  , ValidationOutcome (..)
  , renderValidationOutcome
  , addressValidator
  , strictFormatValidator
  , teamCodeValidator
  , explainFailure
  , preflight
  , metaschemaValidator
  , decodeSchemaInOrder
  , encodeSchema
  , handBuiltSchema
  , main
  ) where

import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as LBS
import Data.Foldable qualified as Foldable
import Data.List.NonEmpty qualified as NEL
import Data.Map.Ordered qualified as OMap
import Data.Set.Ordered qualified as OSet
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Fleece.Aeson qualified as FA
import Fleece.Core qualified as FC

import JsonSchema qualified as Schema
import JsonSchema.Fleece qualified as SchemaFleece
import JsonSchema.Generate qualified as Generate
import JsonSchema.OrderedDecoder qualified as OrderedDecoder
import JsonSchema.Validate qualified as Validate

data Task = Task
  { taskTitle :: T.Text
  , taskTags :: [T.Text]
  , taskEstimate :: Maybe Int
  }
  deriving (Eq, Show)

taskSchema :: (FC.Fleece t) => FC.Schema t Task
taskSchema =
  FC.object
    $ FC.constructor Task
      FC.#+ FC.required "title" taskTitle (FC.minLength 1 FC.text)
      FC.#+ FC.required "tags" taskTags (FC.list FC.text)
      FC.#+ FC.optional "estimate" taskEstimate FC.int

taskJsonSchema :: Schema.Schema
taskJsonSchema = Generate.toJsonSchema taskSchema

taskValidator :: Validate.Validator
taskValidator = Validate.compile Validate.emptyRegistry taskJsonSchema

checkTask :: Aeson.Value -> ValidationOutcome
checkTask =
  maybe Valid Invalid . NEL.nonEmpty . Validate.validate taskValidator

data ValidationOutcome
  = Valid
  | Invalid (NEL.NonEmpty Validate.ValidationError)

renderValidationOutcome :: ValidationOutcome -> T.Text
renderValidationOutcome outcome =
  case outcome of
    Valid -> "valid"
    Invalid errs -> Validate.renderErrors (NEL.toList errs)

addressValidator :: Either String Validate.Validator
addressValidator = do
  address <- decodeJson addressSchemaJson
  person <- decodeJson personSchemaJson
  let
    registry = Validate.addSchema address Validate.emptyRegistry
  pure (Validate.compile registry person)

decodeJson :: LBS.ByteString -> Either String Schema.Schema
decodeJson = Aeson.eitherDecode

addressSchemaJson :: LBS.ByteString
addressSchemaJson =
  "{\"$id\": \"https://example.com/address\", \"type\": \"object\", \"required\": [\"city\"], \"properties\": {\"city\": {\"type\": \"string\"}}}"

personSchemaJson :: LBS.ByteString
personSchemaJson =
  "{\"type\": \"object\", \"properties\": {\"home\": {\"$ref\": \"https://example.com/address\"}}}"

strictFormatValidator :: Either String Validate.Validator
strictFormatValidator = do
  schema <- decodeJson emailSchemaJson
  let
    registry =
      Validate.withFormatAssertion Validate.FormatAssertionAlways Validate.emptyRegistry
  pure (Validate.compile registry schema)

emailSchemaJson :: LBS.ByteString
emailSchemaJson = "{\"type\": \"string\", \"format\": \"email\"}"

teamCodeValidator :: Either String Validate.Validator
teamCodeValidator = do
  schema <- decodeJson "{\"type\": \"string\", \"format\": \"team-code\"}"
  let
    isTeamCode code = T.length code == 3 && T.all (`elem` ['A' .. 'Z']) code
    registry =
      Validate.withFormat
        (Validate.Format "team-code")
        (Validate.FormatPredicate isTeamCode)
        Validate.emptyRegistry
  pure (Validate.compile registry schema)

explainFailure :: Validate.ValidationError -> T.Text
explainFailure err =
  let
    location = Validate.renderJsonPointer (Validate.errorInstancePath err)
   in
    case Validate.errorKind err of
      Validate.RequiredMissing field -> location <> " is missing " <> field
      Validate.AnyOfFailed ->
        location <> " matched no branch: " <> T.intercalate "; " (fmap explainFailure (Validate.errorContext err))
      _ -> Validate.renderValidationError err

preflight :: Validate.Registry -> Schema.Schema -> Either T.Text Validate.Validator
preflight registry schema =
  case Validate.checkSchema registry schema of
    Left err -> Left (Validate.renderSchemaError err)
    Right () -> Right (Validate.compile registry schema)

metaschemaValidator :: Either String Validate.Validator
metaschemaValidator = do
  metaschema <- decodeJson "{\"$ref\": \"https://json-schema.org/draft/2020-12/schema\"}"
  pure (Validate.compile Validate.emptyRegistry metaschema)

decodeSchemaInOrder :: LBS.ByteString -> Either String Schema.Schema
decodeSchemaInOrder = OrderedDecoder.decode SchemaFleece.schemaFleeceSchema

encodeSchema :: Schema.Schema -> LBS.ByteString
encodeSchema = FA.encode SchemaFleece.schemaFleeceSchema

handBuiltSchema :: Schema.Schema
handBuiltSchema =
  Schema.ObjectSchema
    Schema.emptySchema
      { Schema.schemaType = Just (NEL.singleton Schema.TypeObject)
      , Schema.schemaProperties =
          Just
            ( OMap.fromList
                [ ("name", Schema.ObjectSchema Schema.emptySchema{Schema.schemaType = Just (NEL.singleton Schema.TypeString)})
                , ("age", Schema.ObjectSchema Schema.emptySchema{Schema.schemaMinimum = Just 0})
                ]
            )
      , Schema.schemaRequired = Just (OSet.fromList ["name"])
      }

main :: IO ()
main = do
  LBS.putStr (Aeson.encode taskJsonSchema <> "\n")
  TIO.putStrLn (renderValidationOutcome (checkTask (Aeson.object ["title" Aeson..= ("" :: T.Text)])))
  case addressValidator of
    Left err -> putStrLn err
    Right validator -> do
      let
        errs = Validate.validate validator (Aeson.object ["home" Aeson..= Aeson.object []])
      TIO.putStrLn (T.unlines (fmap explainFailure errs))
  Foldable.for_ [strictFormatValidator, teamCodeValidator] $ \result ->
    case result of
      Left err -> putStrLn err
      Right validator -> print (Validate.isValid validator (Aeson.String "nope"))
  case Aeson.eitherDecode "{\"$ref\": \"#/$defs/missing\"}" of
    Left err -> putStrLn err
    Right schema -> case preflight Validate.emptyRegistry schema of
      Left err -> TIO.putStrLn err
      Right _ -> putStrLn "ok"
  case metaschemaValidator of
    Left err -> putStrLn err
    Right validator -> TIO.putStrLn (Validate.renderErrors (Validate.validate validator (Aeson.object ["type" Aeson..= (12 :: Int)])))
  let
    ordered = "{\"properties\": {\"b\": true, \"a\": true}}"
  LBS.putStr (foldMap encodeSchema (decodeSchemaInOrder ordered) <> "\n")
  LBS.putStr (foldMap encodeSchema (decodeJson ordered) <> "\n")
  print
    ( Validate.isValid
        (Validate.compile Validate.emptyRegistry handBuiltSchema)
        (Aeson.object ["name" Aeson..= ("x" :: T.Text), "age" Aeson..= (-1 :: Int)])
    )
