module SchemaGen
  ( genText
  , genUnicodeText
  , genFieldName
  , genScientific
  , genJsonValue
  , genJsonArray
  , genJsonObject
  , genSchemaType
  , genSchema
  ) where

import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Char qualified as Char
import Data.List.NonEmpty qualified as NEL
import Data.Map.Ordered qualified as OMap
import Data.Scientific qualified as Scientific
import Data.Set qualified as Set
import Data.Set.Ordered qualified as OSet
import Data.Text qualified as Text
import Data.Vector qualified as Vector
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import JsonSchema qualified as Schema

genText :: H.Gen Text.Text
genText = Text.pack <$> Gen.string (Range.linear 0 20) Gen.alphaNum

genUnicodeText :: H.Gen Text.Text
genUnicodeText =
  let
    nul = pure '\NUL'
    combining = Gen.enum '\x0300' '\x036F'
    rtl = Gen.element ['\x200F', '\x202B', '\x202E']
    bmp = Gen.unicode
    supplementary = Gen.element . fmap Char.chr $ [0x1F600, 0x1F4A9, 0x1D11E, 0x2070E]
    latin1Accent = Gen.element . fmap Char.chr $ [0x00E9, 0x00FC, 0x00F1, 0x00DF]
    char =
      Gen.frequency
        [ (4, bmp)
        , (2, supplementary)
        , (1, combining)
        , (1, rtl)
        , (1, latin1Accent)
        , (1, nul)
        ]
   in
    Text.pack <$> Gen.list (Range.linear 0 20) char

genFieldName :: H.Gen Text.Text
genFieldName =
  let
    nameChar = Gen.filter (\c -> not (Char.isControl c) && c /= '\NUL') Gen.unicode
   in
    Text.pack <$> Gen.list (Range.linear 1 8) nameChar

genScientific :: H.Gen Scientific.Scientific
genScientific = Scientific.fromFloatDigits <$> Gen.double (Range.linearFrac (-1000) 1000)

genJsonValue :: H.Gen Aeson.Value
genJsonValue =
  Gen.recursive
    Gen.choice
    [ pure Aeson.Null
    , Aeson.Bool <$> Gen.bool
    , Aeson.Number <$> genScientific
    , Aeson.String <$> genText
    ]
    [ Aeson.Array <$> genJsonArray
    , Aeson.Object <$> genJsonObject
    ]

genJsonArray :: H.Gen Aeson.Array
genJsonArray = Vector.fromList <$> Gen.list (Range.linear 0 5) genJsonValue

genJsonObject :: H.Gen Aeson.Object
genJsonObject = do
  keys <- Gen.set (Range.linear 0 4) genFieldName
  values <- Gen.list (Range.singleton (Set.size keys)) genJsonValue
  pure . KeyMap.fromList $ zip (fmap Key.fromText (Set.toList keys)) values

genSchemaType :: H.Gen Schema.SchemaType
genSchemaType =
  Gen.element
    [ Schema.TypeNull
    , Schema.TypeBoolean
    , Schema.TypeObject
    , Schema.TypeArray
    , Schema.TypeNumber
    , Schema.TypeInteger
    , Schema.TypeString
    ]

genSchema :: H.Gen Schema.Schema
genSchema =
  Gen.recursive
    Gen.choice
    [ Schema.BoolSchema <$> Gen.bool
    , pure . Schema.ObjectSchema $ Schema.emptySchema
    , fmap (\ty -> Schema.ObjectSchema Schema.emptySchema{Schema.schemaType = Just (NEL.singleton ty)}) genSchemaType
    , fmap (\v -> Schema.ObjectSchema Schema.emptySchema{Schema.schemaConst = Just v}) genJsonValue
    , fmap
        (\vs -> Schema.ObjectSchema Schema.emptySchema{Schema.schemaEnum = Just vs})
        (Gen.list (Range.linear 0 4) genJsonValue)
    , fmap (\t -> Schema.ObjectSchema Schema.emptySchema{Schema.schemaTitle = Just t}) genText
    , fmap (\t -> Schema.ObjectSchema Schema.emptySchema{Schema.schemaDescription = Just t}) genText
    , genNumericSchema
    , genStringSchema
    , genExtraKeywordsSchema
    ]
    [ genObjectSchema
    , genArraySchema
    , genCompositionSchema
    , genConditionalSchema
    , genDefsSchema
    ]

genNumericSchema :: H.Gen Schema.Schema
genNumericSchema = do
  mn <- Gen.maybe genScientific
  mx <- Gen.maybe genScientific
  emn <- Gen.maybe genScientific
  emx <- Gen.maybe genScientific
  mof <- Gen.maybe (Gen.mapMaybe Schema.mkPositiveScientific genScientific)
  pure
    . Schema.ObjectSchema
    $ Schema.emptySchema
      { Schema.schemaMinimum = mn
      , Schema.schemaMaximum = mx
      , Schema.schemaExclusiveMinimum = emn
      , Schema.schemaExclusiveMaximum = emx
      , Schema.schemaMultipleOf = mof
      }

genStringSchema :: H.Gen Schema.Schema
genStringSchema = do
  minL <- Gen.maybe $ Gen.word (Range.linear 0 50)
  maxL <- Gen.maybe $ Gen.word (Range.linear 0 50)
  pat <- Gen.maybe genSafePattern
  fmt <- Gen.maybe genFormat
  pure
    . Schema.ObjectSchema
    $ Schema.emptySchema
      { Schema.schemaMinLength = minL
      , Schema.schemaMaxLength = maxL
      , Schema.schemaPattern = pat
      , Schema.schemaFormat = fmt
      }

genSafePattern :: H.Gen Text.Text
genSafePattern =
  Gen.element . fmap Text.pack
    $ ["^[a-z]+$", "^[A-Z][a-z]*$", "[0-9]+", "^.*$", "a", "x{1,3}", "(foo|bar)"]

genFormat :: H.Gen Text.Text
genFormat =
  Gen.element . fmap Text.pack
    $ [ "date"
      , "date-time"
      , "time"
      , "email"
      , "uuid"
      , "uri"
      , "uri-reference"
      , "ipv4"
      , "ipv6"
      , "regex"
      , "unknown-format"
      , "x-custom"
      ]

genExtraKeywordsSchema :: H.Gen Schema.Schema
genExtraKeywordsSchema = do
  extras <- Gen.map (Range.linear 1 3) ((,) <$> fmap (Text.pack "x-" <>) genFieldName <*> genJsonValue)
  pure . Schema.ObjectSchema $ Schema.emptySchema{Schema.schemaExtraKeywords = extras}

genObjectSchema :: H.Gen Schema.Schema
genObjectSchema = do
  props <- Gen.maybe (genPropertyMap genSchema)
  required <- Gen.maybe . fmap OSet.fromList . Gen.list (Range.linear 0 3) $ genFieldName
  addProps <- Gen.maybe genSchema
  patProps <- Gen.maybe $ do
    keys <- Gen.set (Range.linear 0 2) genSafePattern
    vals <- Gen.list (Range.singleton (Set.size keys)) genSchema
    pure . OMap.fromList $ zip (Set.toList keys) vals
  propNames <- Gen.maybe genSchema
  minP <- Gen.maybe $ Gen.word (Range.linear 0 5)
  maxP <- Gen.maybe $ Gen.word (Range.linear 0 5)
  unevalProps <- Gen.maybe genSchema
  pure
    . Schema.ObjectSchema
    $ Schema.emptySchema
      { Schema.schemaProperties = props
      , Schema.schemaRequired = required
      , Schema.schemaAdditionalProperties = addProps
      , Schema.schemaPatternProperties = patProps
      , Schema.schemaPropertyNames = propNames
      , Schema.schemaMinProperties = minP
      , Schema.schemaMaxProperties = maxP
      , Schema.schemaUnevaluatedProperties = unevalProps
      }

genPropertyMap :: H.Gen Schema.Schema -> H.Gen (OMap.OMap Text.Text Schema.Schema)
genPropertyMap inner = do
  keys <- Gen.set (Range.linear 0 3) genFieldName
  values <- Gen.list (Range.singleton (Set.size keys)) inner
  pure . OMap.fromList $ zip (Set.toList keys) values

genArraySchema :: H.Gen Schema.Schema
genArraySchema = do
  items <- Gen.maybe genSchema
  prefix <- Gen.maybe . Gen.nonEmpty (Range.linear 1 3) $ genSchema
  contains <- Gen.maybe genSchema
  minI <- Gen.maybe $ Gen.word (Range.linear 0 10)
  maxI <- Gen.maybe $ Gen.word (Range.linear 0 10)
  unique <- Gen.maybe Gen.bool
  unevalItems <- Gen.maybe genSchema
  pure
    . Schema.ObjectSchema
    $ Schema.emptySchema
      { Schema.schemaItems = items
      , Schema.schemaPrefixItems = prefix
      , Schema.schemaContains = contains
      , Schema.schemaMinItems = minI
      , Schema.schemaMaxItems = maxI
      , Schema.schemaUniqueItems = unique
      , Schema.schemaUnevaluatedItems = unevalItems
      }

genCompositionSchema :: H.Gen Schema.Schema
genCompositionSchema = do
  schemas <- Gen.nonEmpty (Range.linear 1 3) genSchema
  notS <- Gen.maybe genSchema
  Gen.element
    [ Schema.ObjectSchema Schema.emptySchema{Schema.schemaAllOf = Just schemas, Schema.schemaNot = notS}
    , Schema.ObjectSchema Schema.emptySchema{Schema.schemaAnyOf = Just schemas, Schema.schemaNot = notS}
    , Schema.ObjectSchema Schema.emptySchema{Schema.schemaOneOf = Just schemas, Schema.schemaNot = notS}
    ]

genConditionalSchema :: H.Gen Schema.Schema
genConditionalSchema = do
  ifS <- genSchema
  thenS <- Gen.maybe genSchema
  elseS <- Gen.maybe genSchema
  pure
    . Schema.ObjectSchema
    $ Schema.emptySchema
      { Schema.schemaIf = Just ifS
      , Schema.schemaThen = thenS
      , Schema.schemaElse = elseS
      }

genDefsSchema :: H.Gen Schema.Schema
genDefsSchema = do
  defs <- genPropertyMap genSchema
  body <- Gen.maybe genSchema
  pure
    . Schema.ObjectSchema
    $ Schema.emptySchema
      { Schema.schemaDefs = Just defs
      , Schema.schemaItems = body
      }
