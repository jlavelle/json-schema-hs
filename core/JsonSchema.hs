module JsonSchema
  ( Schema (..)
  , SchemaObject
  , SchemaObjectF (..)
  , SchemaType (..)
  , Dependency
  , DependencyF (..)
  , PositiveScientific
  , mkPositiveScientific
  , positiveScientificToScientific
  , SchemaId (..)
  , Anchor (..)
  , DynamicAnchor (..)
  , SchemaRef (..)
  , DynamicRef (..)
  , MetaschemaUri (..)
  , schemaTypeToText
  , validateMultipleOf
  , validateRequired
  , validateSchemaId
  , validateAnchorName
  , emptySchema
  , emptySchemaF
  ) where

import Control.DeepSeq qualified as DeepSeq
import Data.Aeson qualified as Aeson
import Data.Aeson.Encoding qualified as Encoding
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types qualified as AesonTypes
import Data.Bifunctor qualified as Bifunctor
import Data.Char qualified as Char
import Data.Foldable qualified as Foldable
import Data.List.NonEmpty qualified as NEL
import Data.Map.Ordered qualified as OMap
import Data.Map.Strict qualified as Map
import Data.Scientific qualified as Scientific
import Data.Set.Ordered qualified as OSet
import Data.Text qualified as Text
import GHC.Generics qualified as Generics

data Schema
  = BoolSchema !Bool
  | ObjectSchema !SchemaObject
  deriving stock (Eq, Show)

instance Aeson.FromJSON Schema where
  parseJSON value = case value of
    Aeson.Bool b -> pure (BoolSchema b)
    Aeson.Object obj -> ObjectSchema <$> parseSchemaObject obj
    _ -> AesonTypes.typeMismatch "Schema" value

instance Aeson.ToJSON Schema where
  toJSON schema = case schema of
    BoolSchema b -> Aeson.Bool b
    ObjectSchema obj -> Aeson.Object (schemaObjectFields obj)
  toEncoding schema = case schema of
    BoolSchema b -> Encoding.bool b
    ObjectSchema obj -> Encoding.pairs (schemaObjectFields obj)

type SchemaObject = SchemaObjectF Schema

data SchemaObjectF r = SchemaObjectF
  { schemaType :: !(Maybe (NEL.NonEmpty SchemaType))
  , schemaEnum :: !(Maybe [Aeson.Value])
  , schemaConst :: !(Maybe Aeson.Value)
  , schemaProperties :: !(Maybe (OMap.OMap Text.Text r))
  , schemaRequired :: !(Maybe (OSet.OSet Text.Text))
  , schemaAdditionalProperties :: !(Maybe r)
  , schemaPatternProperties :: !(Maybe (OMap.OMap Text.Text r))
  , schemaPropertyNames :: !(Maybe r)
  , schemaMinProperties :: !(Maybe Word)
  , schemaMaxProperties :: !(Maybe Word)
  , schemaItems :: !(Maybe r)
  , schemaPrefixItems :: !(Maybe (NEL.NonEmpty r))
  , schemaContains :: !(Maybe r)
  , schemaMinContains :: !(Maybe Word)
  , schemaMaxContains :: !(Maybe Word)
  , schemaMinItems :: !(Maybe Word)
  , schemaMaxItems :: !(Maybe Word)
  , schemaUniqueItems :: !(Maybe Bool)
  , schemaMinLength :: !(Maybe Word)
  , schemaMaxLength :: !(Maybe Word)
  , schemaPattern :: !(Maybe Text.Text)
  , schemaFormat :: !(Maybe Text.Text)
  , schemaMinimum :: !(Maybe Scientific.Scientific)
  , schemaMaximum :: !(Maybe Scientific.Scientific)
  , schemaExclusiveMinimum :: !(Maybe Scientific.Scientific)
  , schemaExclusiveMaximum :: !(Maybe Scientific.Scientific)
  , schemaMultipleOf :: !(Maybe PositiveScientific)
  , schemaAllOf :: !(Maybe (NEL.NonEmpty r))
  , schemaAnyOf :: !(Maybe (NEL.NonEmpty r))
  , schemaOneOf :: !(Maybe (NEL.NonEmpty r))
  , schemaNot :: !(Maybe r)
  , schemaIf :: !(Maybe r)
  , schemaThen :: !(Maybe r)
  , schemaElse :: !(Maybe r)
  , schemaDependentSchemas :: !(Maybe (OMap.OMap Text.Text r))
  , schemaDependentRequired :: !(Maybe (OMap.OMap Text.Text [Text.Text]))
  , schemaId :: !(Maybe SchemaId)
  , schemaAnchor :: !(Maybe Anchor)
  , schemaDynamicAnchor :: !(Maybe DynamicAnchor)
  , schemaRef :: !(Maybe SchemaRef)
  , schemaDynamicRef :: !(Maybe DynamicRef)
  , schemaDefs :: !(Maybe (OMap.OMap Text.Text r))
  , schemaUnevaluatedProperties :: !(Maybe r)
  , schemaUnevaluatedItems :: !(Maybe r)
  , schemaDependencies :: !(Maybe (OMap.OMap Text.Text (DependencyF r)))
  , schemaTitle :: !(Maybe Text.Text)
  , schemaDescription :: !(Maybe Text.Text)
  , schemaMetaschema :: !(Maybe MetaschemaUri)
  , schemaVocabulary :: !(Maybe (Map.Map Text.Text Bool))
  , schemaExtraKeywords :: !(Map.Map Text.Text Aeson.Value)
  }
  deriving stock (Eq, Foldable, Functor, Show, Traversable)

data SchemaType
  = TypeNull
  | TypeBoolean
  | TypeObject
  | TypeArray
  | TypeNumber
  | TypeInteger
  | TypeString
  deriving stock (Bounded, Enum, Eq, Generics.Generic, Ord, Show)
  deriving anyclass (DeepSeq.NFData)

newtype PositiveScientific = PositiveScientific Scientific.Scientific
  deriving stock (Eq, Ord, Show)

newtype SchemaId = SchemaId {schemaIdToText :: Text.Text}
  deriving stock (Eq, Ord, Show)

newtype Anchor = Anchor {anchorToText :: Text.Text}
  deriving stock (Eq, Ord, Show)

newtype DynamicAnchor = DynamicAnchor {dynamicAnchorToText :: Text.Text}
  deriving stock (Eq, Ord, Show)

newtype SchemaRef = SchemaRef {schemaRefToText :: Text.Text}
  deriving stock (Eq, Ord, Show)

newtype DynamicRef = DynamicRef {dynamicRefToText :: Text.Text}
  deriving stock (Eq, Ord, Show)

data DependencyF r
  = PropertyDependency ![Text.Text]
  | SchemaDependency !r
  deriving stock (Eq, Foldable, Functor, Show, Traversable)

newtype MetaschemaUri = MetaschemaUri {metaschemaUriToText :: Text.Text}
  deriving stock (Eq, Ord, Show)

parseSchemaObject :: Aeson.Object -> AesonTypes.Parser SchemaObject
parseSchemaObject obj =
  let
    ObjectParser _ parseKeywords = schemaObjectParser
   in
    parseKeywords obj <*> pure (KeyMap.toMapText (KeyMap.difference obj schemaKeywords))

data ObjectParser a = ObjectParser [Key.Key] (Aeson.Object -> AesonTypes.Parser a)

instance Functor ObjectParser where
  fmap f (ObjectParser keys parse) = ObjectParser keys (fmap f . parse)

instance Applicative ObjectParser where
  pure a = ObjectParser [] (const (pure a))
  ObjectParser keysF parseF <*> ObjectParser keysA parseA =
    ObjectParser (keysF <> keysA) (\obj -> parseF obj <*> parseA obj)

schemaObjectParser :: ObjectParser (Map.Map Text.Text Aeson.Value -> SchemaObject)
schemaObjectParser =
  SchemaObjectF
    <$> keyword "type" parseSchemaTypes
    <*> keyword "enum" Aeson.parseJSON
    <*> keyword "const" pure
    <*> keyword "properties" (parseOrderedObject Aeson.parseJSON)
    <*> keyword "required" parseRequired
    <*> keyword "additionalProperties" Aeson.parseJSON
    <*> keyword "patternProperties" (parseOrderedObject Aeson.parseJSON)
    <*> keyword "propertyNames" Aeson.parseJSON
    <*> keyword "minProperties" Aeson.parseJSON
    <*> keyword "maxProperties" Aeson.parseJSON
    <*> keyword "items" Aeson.parseJSON
    <*> keyword "prefixItems" Aeson.parseJSON
    <*> keyword "contains" Aeson.parseJSON
    <*> keyword "minContains" Aeson.parseJSON
    <*> keyword "maxContains" Aeson.parseJSON
    <*> keyword "minItems" Aeson.parseJSON
    <*> keyword "maxItems" Aeson.parseJSON
    <*> keyword "uniqueItems" Aeson.parseJSON
    <*> keyword "minLength" Aeson.parseJSON
    <*> keyword "maxLength" Aeson.parseJSON
    <*> keyword "pattern" Aeson.parseJSON
    <*> keyword "format" Aeson.parseJSON
    <*> keyword "minimum" Aeson.parseJSON
    <*> keyword "maximum" Aeson.parseJSON
    <*> keyword "exclusiveMinimum" Aeson.parseJSON
    <*> keyword "exclusiveMaximum" Aeson.parseJSON
    <*> keyword "multipleOf" parseMultipleOf
    <*> keyword "allOf" Aeson.parseJSON
    <*> keyword "anyOf" Aeson.parseJSON
    <*> keyword "oneOf" Aeson.parseJSON
    <*> keyword "not" Aeson.parseJSON
    <*> keyword "if" Aeson.parseJSON
    <*> keyword "then" Aeson.parseJSON
    <*> keyword "else" Aeson.parseJSON
    <*> keyword "dependentSchemas" (parseOrderedObject Aeson.parseJSON)
    <*> keyword "dependentRequired" (parseOrderedObject Aeson.parseJSON)
    <*> keyword "$id" parseSchemaId
    <*> keyword "$anchor" (parseAnchor Anchor)
    <*> keyword "$dynamicAnchor" (parseAnchor DynamicAnchor)
    <*> keyword "$ref" (fmap SchemaRef . Aeson.parseJSON)
    <*> keyword "$dynamicRef" (fmap DynamicRef . Aeson.parseJSON)
    <*> keyword "$defs" (parseOrderedObject Aeson.parseJSON)
    <*> keyword "unevaluatedProperties" Aeson.parseJSON
    <*> keyword "unevaluatedItems" Aeson.parseJSON
    <*> keyword "dependencies" (parseOrderedObject parseDependency)
    <*> keyword "title" Aeson.parseJSON
    <*> keyword "description" Aeson.parseJSON
    <*> keyword "$schema" (fmap MetaschemaUri . Aeson.parseJSON)
    <*> keyword "$vocabulary" Aeson.parseJSON

keyword :: Key.Key -> (Aeson.Value -> AesonTypes.Parser a) -> ObjectParser (Maybe a)
keyword key parse = ObjectParser [key] (\obj -> AesonTypes.explicitParseFieldMaybe' parse obj key)

parseSchemaTypes :: Aeson.Value -> AesonTypes.Parser (NEL.NonEmpty SchemaType)
parseSchemaTypes value = case value of
  Aeson.String _ -> NEL.singleton <$> parseSchemaType value
  _ -> traverse parseSchemaType =<< Aeson.parseJSON value

parseSchemaType :: Aeson.Value -> AesonTypes.Parser SchemaType
parseSchemaType =
  let
    byName = Map.fromList [(schemaTypeToText ty, ty) | ty <- [minBound .. maxBound]]
   in
    Aeson.withText "SchemaType" $ \name ->
      maybe (fail ("unrecognized type name: " <> show name)) pure (Map.lookup name byName)

schemaTypeToText :: SchemaType -> Text.Text
schemaTypeToText ty = case ty of
  TypeNull -> "null"
  TypeBoolean -> "boolean"
  TypeObject -> "object"
  TypeArray -> "array"
  TypeNumber -> "number"
  TypeInteger -> "integer"
  TypeString -> "string"

parseOrderedObject :: (Aeson.Value -> AesonTypes.Parser a) -> Aeson.Value -> AesonTypes.Parser (OMap.OMap Text.Text a)
parseOrderedObject parse =
  let
    parseEntry (key, value) = (,) (Key.toText key) <$> parse value AesonTypes.<?> AesonTypes.Key key
   in
    Aeson.withObject "ordered properties" (fmap OMap.fromList . traverse parseEntry . KeyMap.toList)

parseRequired :: Aeson.Value -> AesonTypes.Parser (OSet.OSet Text.Text)
parseRequired value = do
  names <- Aeson.parseJSON value
  either fail pure (validateRequired names)

validateRequired :: [Text.Text] -> Either String (OSet.OSet Text.Text)
validateRequired names =
  let
    nameSet = OSet.fromList names
   in
    if OSet.size nameSet == length names
      then Right nameSet
      else Left "required must not contain duplicate property names"

parseMultipleOf :: Aeson.Value -> AesonTypes.Parser PositiveScientific
parseMultipleOf value =
  either fail pure . validateMultipleOf =<< Aeson.parseJSON value

validateMultipleOf :: Scientific.Scientific -> Either String PositiveScientific
validateMultipleOf =
  maybe (Left "multipleOf must be strictly greater than zero") Right . mkPositiveScientific

mkPositiveScientific :: Scientific.Scientific -> Maybe PositiveScientific
mkPositiveScientific s
  | s > 0 = Just (PositiveScientific s)
  | otherwise = Nothing

parseSchemaId :: Aeson.Value -> AesonTypes.Parser SchemaId
parseSchemaId = Aeson.withText "$id" (either fail pure . validateSchemaId)

validateSchemaId :: Text.Text -> Either String SchemaId
validateSchemaId uri
  | Text.length (snd (Text.breakOn "#" uri)) > 1 = Left "$id must not contain a non-empty fragment"
  | otherwise = Right (SchemaId uri)

parseAnchor :: (Text.Text -> a) -> Aeson.Value -> AesonTypes.Parser a
parseAnchor fromText = Aeson.withText "anchor" (either fail (pure . fromText) . validateAnchorName)

validateAnchorName :: Text.Text -> Either String Text.Text
validateAnchorName name = case Text.uncons name of
  Just (first, rest)
    | isAnchorStart first && Text.all isAnchorChar rest -> Right name
  _ -> Left ("invalid anchor name: " <> Text.unpack name)

isAnchorStart :: Char -> Bool
isAnchorStart c = Char.isAsciiUpper c || Char.isAsciiLower c || c == '_'

isAnchorChar :: Char -> Bool
isAnchorChar c = isAnchorStart c || Char.isDigit c || c == '-' || c == '.'

parseDependency :: Aeson.Value -> AesonTypes.Parser Dependency
parseDependency value = case value of
  Aeson.Array _ -> PropertyDependency <$> Aeson.parseJSON value
  _ -> SchemaDependency <$> Aeson.parseJSON value

type Dependency = DependencyF Schema

schemaKeywords :: KeyMap.KeyMap ()
schemaKeywords =
  let
    ObjectParser keys _ = schemaObjectParser
   in
    KeyMap.fromList (fmap (\key -> (key, ())) keys)

schemaObjectFields :: (Aeson.KeyValueOmit e kv, Monoid kv) => SchemaObject -> kv
schemaObjectFields obj =
  Foldable.fold
    [ "type" Aeson..?= fmap schemaTypesToJSON (schemaType obj)
    , "enum" Aeson..?= schemaEnum obj
    , "const" Aeson..?= schemaConst obj
    , "properties" Aeson..?= fmap OrderedObject (schemaProperties obj)
    , "required" Aeson..?= fmap Foldable.toList (schemaRequired obj)
    , "additionalProperties" Aeson..?= schemaAdditionalProperties obj
    , "patternProperties" Aeson..?= fmap OrderedObject (schemaPatternProperties obj)
    , "propertyNames" Aeson..?= schemaPropertyNames obj
    , "minProperties" Aeson..?= schemaMinProperties obj
    , "maxProperties" Aeson..?= schemaMaxProperties obj
    , "items" Aeson..?= schemaItems obj
    , "prefixItems" Aeson..?= schemaPrefixItems obj
    , "contains" Aeson..?= schemaContains obj
    , "minContains" Aeson..?= schemaMinContains obj
    , "maxContains" Aeson..?= schemaMaxContains obj
    , "minItems" Aeson..?= schemaMinItems obj
    , "maxItems" Aeson..?= schemaMaxItems obj
    , "uniqueItems" Aeson..?= schemaUniqueItems obj
    , "minLength" Aeson..?= schemaMinLength obj
    , "maxLength" Aeson..?= schemaMaxLength obj
    , "pattern" Aeson..?= schemaPattern obj
    , "format" Aeson..?= schemaFormat obj
    , "minimum" Aeson..?= schemaMinimum obj
    , "maximum" Aeson..?= schemaMaximum obj
    , "exclusiveMinimum" Aeson..?= schemaExclusiveMinimum obj
    , "exclusiveMaximum" Aeson..?= schemaExclusiveMaximum obj
    , "multipleOf" Aeson..?= fmap positiveScientificToScientific (schemaMultipleOf obj)
    , "allOf" Aeson..?= schemaAllOf obj
    , "anyOf" Aeson..?= schemaAnyOf obj
    , "oneOf" Aeson..?= schemaOneOf obj
    , "not" Aeson..?= schemaNot obj
    , "if" Aeson..?= schemaIf obj
    , "then" Aeson..?= schemaThen obj
    , "else" Aeson..?= schemaElse obj
    , "dependentSchemas" Aeson..?= fmap OrderedObject (schemaDependentSchemas obj)
    , "dependentRequired" Aeson..?= fmap OrderedObject (schemaDependentRequired obj)
    , "$id" Aeson..?= fmap schemaIdToText (schemaId obj)
    , "$anchor" Aeson..?= fmap anchorToText (schemaAnchor obj)
    , "$dynamicAnchor" Aeson..?= fmap dynamicAnchorToText (schemaDynamicAnchor obj)
    , "$ref" Aeson..?= fmap schemaRefToText (schemaRef obj)
    , "$dynamicRef" Aeson..?= fmap dynamicRefToText (schemaDynamicRef obj)
    , "$defs" Aeson..?= fmap OrderedObject (schemaDefs obj)
    , "unevaluatedProperties" Aeson..?= schemaUnevaluatedProperties obj
    , "unevaluatedItems" Aeson..?= schemaUnevaluatedItems obj
    , "dependencies" Aeson..?= fmap (OrderedObject . fmap EncodedDependency) (schemaDependencies obj)
    , "title" Aeson..?= schemaTitle obj
    , "description" Aeson..?= schemaDescription obj
    , "$schema" Aeson..?= fmap metaschemaUriToText (schemaMetaschema obj)
    , "$vocabulary" Aeson..?= schemaVocabulary obj
    , Map.foldMapWithKey (\key value -> Key.fromText key Aeson..= value) (schemaExtraKeywords obj)
    ]

newtype OrderedObject a = OrderedObject (OMap.OMap Text.Text a)

instance (Aeson.ToJSON a) => Aeson.ToJSON (OrderedObject a) where
  toJSON (OrderedObject entries) =
    Aeson.Object . KeyMap.fromList . fmap (Bifunctor.bimap Key.fromText Aeson.toJSON) $ OMap.assocs entries
  toEncoding (OrderedObject entries) =
    Encoding.pairs . foldMap (\(key, value) -> Key.fromText key Aeson..= value) $ OMap.assocs entries

newtype EncodedDependency = EncodedDependency Dependency

instance Aeson.ToJSON EncodedDependency where
  toJSON (EncodedDependency dep) = case dep of
    PropertyDependency keys -> Aeson.toJSON keys
    SchemaDependency schema -> Aeson.toJSON schema
  toEncoding (EncodedDependency dep) = case dep of
    PropertyDependency keys -> Aeson.toEncoding keys
    SchemaDependency schema -> Aeson.toEncoding schema

schemaTypesToJSON :: NEL.NonEmpty SchemaType -> Aeson.Value
schemaTypesToJSON types = case types of
  single NEL.:| [] -> Aeson.String (schemaTypeToText single)
  _ -> Aeson.toJSON (fmap schemaTypeToText types)

positiveScientificToScientific :: PositiveScientific -> Scientific.Scientific
positiveScientificToScientific (PositiveScientific s) = s

emptySchema :: SchemaObject
emptySchema = emptySchemaF

emptySchemaF :: SchemaObjectF r
emptySchemaF =
  SchemaObjectF
    { schemaType = Nothing
    , schemaEnum = Nothing
    , schemaConst = Nothing
    , schemaProperties = Nothing
    , schemaRequired = Nothing
    , schemaAdditionalProperties = Nothing
    , schemaPatternProperties = Nothing
    , schemaPropertyNames = Nothing
    , schemaMinProperties = Nothing
    , schemaMaxProperties = Nothing
    , schemaItems = Nothing
    , schemaPrefixItems = Nothing
    , schemaContains = Nothing
    , schemaMinContains = Nothing
    , schemaMaxContains = Nothing
    , schemaMinItems = Nothing
    , schemaMaxItems = Nothing
    , schemaUniqueItems = Nothing
    , schemaMinLength = Nothing
    , schemaMaxLength = Nothing
    , schemaPattern = Nothing
    , schemaFormat = Nothing
    , schemaMinimum = Nothing
    , schemaMaximum = Nothing
    , schemaExclusiveMinimum = Nothing
    , schemaExclusiveMaximum = Nothing
    , schemaMultipleOf = Nothing
    , schemaAllOf = Nothing
    , schemaAnyOf = Nothing
    , schemaOneOf = Nothing
    , schemaNot = Nothing
    , schemaIf = Nothing
    , schemaThen = Nothing
    , schemaElse = Nothing
    , schemaDependentSchemas = Nothing
    , schemaDependentRequired = Nothing
    , schemaId = Nothing
    , schemaAnchor = Nothing
    , schemaDynamicAnchor = Nothing
    , schemaRef = Nothing
    , schemaDynamicRef = Nothing
    , schemaDefs = Nothing
    , schemaUnevaluatedProperties = Nothing
    , schemaUnevaluatedItems = Nothing
    , schemaDependencies = Nothing
    , schemaTitle = Nothing
    , schemaDescription = Nothing
    , schemaMetaschema = Nothing
    , schemaVocabulary = Nothing
    , schemaExtraKeywords = Map.empty
    }
