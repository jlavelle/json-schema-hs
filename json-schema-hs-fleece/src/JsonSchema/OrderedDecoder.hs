module JsonSchema.OrderedDecoder
  ( Decoder
  , decode
  , decodeStrict
  ) where

import Control.Applicative qualified as Applicative
import Control.Monad qualified as Monad
import Data.Aeson.Decoding.ByteString qualified as DecodingBS
import Data.Aeson.Decoding.ByteString.Lazy qualified as DecodingLBS
import Data.Aeson.Decoding.Tokens qualified as Tokens
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types qualified as AesonTypes
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Ordered qualified as OMap
import Data.Map.Strict qualified as Map
import Data.Scientific qualified as Scientific
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Encoding
import Data.Vector qualified as Vector
import Data.Word qualified as Word
import Fleece.Core qualified as FC
import GHC.TypeLits qualified as TypeLits
import Shrubbery qualified

import JsonSchema.OrderedProperties qualified as OrderedProperties

newtype Decoder a
  = Decoder (Value -> AesonTypes.Parser a)

instance OrderedProperties.OrderedProperties Decoder where
  orderedProperties schema =
    let
      Decoder parseValue = FC.schemaInterpreter schema
      annName = FC.annotateName (FC.schemaName schema) "ordered properties"
      parsePair (key, value) = (,) (Key.toText key) <$> parseField parseValue key value
     in
      FC.Schema
        annName
        ( Decoder
            ( withObject
                (FC.nameToString annName)
                (fmap OMap.fromList . traverse parsePair . membersPairs)
            )
        )

instance FC.Fleece Decoder where
  data Object Decoder _object a = Object
    { objectFields :: [Key.Key]
    , objectDecoder :: Members -> AesonTypes.Parser a
    }

  data Field Decoder _object a = Field
    { fieldName :: Key.Key
    , fieldDecoder :: Members -> AesonTypes.Parser a
    }

  newtype AdditionalFields Decoder _object a = AdditionalFields
    { additionalFieldsDecoder :: [Key.Key] -> Members -> AesonTypes.Parser a
    }

  newtype UnionMembers Decoder allTypes _handledTypes
    = UnionMembers (FC.Name -> Value -> AesonTypes.Parser (Shrubbery.Union allTypes))

  newtype TaggedUnionMembers Decoder allTags _handledTags
    = TaggedUnionMembers (Map.Map Text.Text (Members -> AesonTypes.Parser (Shrubbery.TaggedUnion allTags)))

  interpretFormat _ =
    FC.schemaInterpreter

  interpretNumber name =
    Decoder $ \value -> case value of
      NumberValue number -> pure number
      _ -> typeMismatch (FC.nameToString name) "Number" value

  interpretText name =
    Decoder (withText (FC.nameToString name) pure)

  interpretBoolean name =
    Decoder $ \value -> case value of
      BoolValue bool -> pure bool
      _ -> typeMismatch (FC.nameToString name) "Boolean" value

  interpretArray name schema =
    let
      Decoder parseItem = FC.schemaInterpreter schema
     in
      Decoder $ \value -> case value of
        ArrayValue items -> Vector.imapM (\i item -> parseItem item AesonTypes.<?> AesonTypes.Index i) items
        _ -> typeMismatch (FC.nameToString name) "Array" value

  interpretNull name =
    Decoder $ \value -> case value of
      NullValue -> pure FC.Null
      _ -> typeMismatch (FC.nameToString name) "Null" value

  interpretNullable _name schema =
    let
      Decoder parseValue = FC.schemaInterpreter schema
     in
      Decoder $ \value -> case value of
        NullValue -> pure (Left FC.Null)
        _ -> Right <$> parseValue value

  required name _accessor schema =
    let
      key = Key.fromString name
      Decoder parseValue = FC.schemaInterpreter schema
     in
      Field key (requiredField parseValue key)

  optional name _accessor schema =
    let
      key = Key.fromString name
      Decoder parseValue = FC.schemaInterpreter schema
     in
      Field key $ \object ->
        traverse (parseField parseValue key) (KeyMap.lookup key (membersMap object))

  additionalFields _accessor schema =
    let
      Decoder parseValue = FC.schemaInterpreter schema
     in
      AdditionalFields $ \definedFields object ->
        fmap KeyMap.toMapText
          . KeyMap.traverseWithKey (parseField parseValue)
          . KeyMap.filterWithKey (\key _value -> key `notElem` definedFields)
          $ membersMap object

  mapField f (Field name parseFieldValue) =
    Field name (fmap f . parseFieldValue)

  constructor f =
    Object [] (\_object -> pure f)

  field (Object fieldNames parseF) objectField =
    Object
      { objectFields = fieldName objectField : fieldNames
      , objectDecoder = \object -> parseF object <*> fieldDecoder objectField object
      }

  additional (Object fieldNames parseF) fields =
    Object
      { objectFields = fieldNames
      , objectDecoder = \object -> parseF object <*> additionalFieldsDecoder fields fieldNames object
      }

  interpretObjectNamed name (Object _fieldNames parseObject) =
    Decoder (withObject (FC.nameToString name) parseObject)

  interpretBoundedEnumNamed name toText =
    let
      decodingMap = Map.fromList . fmap (\e -> (toText e, e)) $ [minBound .. maxBound]
     in
      Decoder . withText (FC.nameToString name) $ \text ->
        case Map.lookup text decodingMap of
          Just enumValue -> pure enumValue
          Nothing -> fail ("Unrecognized value for " <> FC.nameToString name <> " enum: " <> show text)

  interpretValidateNamed name _uncheck =
    validatingDecoder name

  interpretValidateAnonymous _uncheck check schema =
    validatingDecoder (FC.schemaName schema) check schema

  interpretUnionNamed name (UnionMembers parseMembers) =
    Decoder (parseMembers name)

  unionMemberWithIndex index schema =
    let
      Decoder parseMember = FC.schemaInterpreter schema
     in
      UnionMembers (\_name -> fmap (Shrubbery.unifyUnion index) . parseMember)

  unionCombine (UnionMembers parseLeft) (UnionMembers parseRight) =
    UnionMembers $ \name value ->
      parseLeft name value
        Applicative.<|> parseRight name value
        Applicative.<|> fail ("All union parsing options for " <> FC.nameUnqualified name <> " failed.")

  interpretTaggedUnionNamed name tagProperty (TaggedUnionMembers parserMap) =
    let
      tagKey = Key.fromString tagProperty
      nameString = FC.nameToString name
      parseTag = withText (tagProperty <> " field of " <> nameString) pure
     in
      Decoder . withObject nameString $ \object -> do
        tagValue <- requiredField parseTag tagKey object
        case Map.lookup tagValue parserMap of
          Just parseObject -> parseObject object
          Nothing -> fail ("Invalid tag found for tagged union " <> nameString <> ": " <> Text.unpack tagValue)

  taggedUnionMemberWithTag ::
    forall tag allTags a proxy n.
    ( Shrubbery.LookupTag tag allTags ~ '(n, a)
    , TypeLits.KnownNat n
    , TypeLits.KnownSymbol tag
    ) =>
    proxy tag ->
    FC.Object Decoder a a ->
    FC.TaggedUnionMembers Decoder allTags '[tag Shrubbery.@= a]
  taggedUnionMemberWithTag tag object =
    TaggedUnionMembers
      ( Map.singleton
          (Text.pack (TypeLits.symbolVal tag))
          (fmap (Shrubbery.unifyTaggedUnion @tag) . objectDecoder object)
      )

  taggedUnionCombine (TaggedUnionMembers left) (TaggedUnionMembers right) =
    TaggedUnionMembers (Map.union left right)

  interpretJsonString schema =
    let
      Decoder parseValue = FC.schemaInterpreter schema
     in
      Decoder . withText (FC.nameUnqualified (FC.schemaName schema)) $ \jsonText ->
        case parseStrict (Encoding.encodeUtf8 jsonText) of
          Left err -> fail ("Error decoding nested json string:" <> err)
          Right value -> parseValue value

data Value
  = ObjectValue !Members
  | ArrayValue !(Vector.Vector Value)
  | StringValue !Text.Text
  | NumberValue !Scientific.Scientific
  | BoolValue !Bool
  | NullValue

data Members = Members
  { membersPairs :: [(Key.Key, Value)]
  , membersMap :: !(KeyMap.KeyMap Value)
  }

parseField :: (Value -> AesonTypes.Parser a) -> Key.Key -> Value -> AesonTypes.Parser a
parseField parseValue key value =
  parseValue value AesonTypes.<?> AesonTypes.Key key

withObject :: String -> (Members -> AesonTypes.Parser a) -> Value -> AesonTypes.Parser a
withObject name parse value = case value of
  ObjectValue object -> parse object
  _ -> typeMismatch name "Object" value

typeMismatch :: String -> String -> Value -> AesonTypes.Parser a
typeMismatch name expected value =
  fail ("parsing " <> name <> " failed, expected " <> expected <> ", but encountered " <> valueKind value)

valueKind :: Value -> String
valueKind value = case value of
  ObjectValue _ -> "Object"
  ArrayValue _ -> "Array"
  StringValue _ -> "String"
  NumberValue _ -> "Number"
  BoolValue _ -> "Boolean"
  NullValue -> "Null"

withText :: String -> (Text.Text -> AesonTypes.Parser a) -> Value -> AesonTypes.Parser a
withText name parse value = case value of
  StringValue text -> parse text
  _ -> typeMismatch name "String" value

requiredField :: (Value -> AesonTypes.Parser a) -> Key.Key -> Members -> AesonTypes.Parser a
requiredField parseValue key object =
  case KeyMap.lookup key (membersMap object) of
    Just value -> parseField parseValue key value
    Nothing -> fail ("key " <> show key <> " not found")

validatingDecoder :: FC.Name -> (a -> Either String b) -> FC.Schema Decoder a -> Decoder b
validatingDecoder name check schema =
  let
    Decoder parseValue = FC.schemaInterpreter schema
   in
    Decoder $ \value -> do
      unchecked <- parseValue value
      case check unchecked of
        Right checked -> pure checked
        Left err -> fail ("Error validating " <> FC.nameToString name <> ": " <> err)

parseStrict :: BS.ByteString -> Either String Value
parseStrict =
  valueFromTokens (BS.all isJsonSpace) . DecodingBS.bsToTokens

valueFromTokens :: (k -> Bool) -> Tokens.Tokens k String -> Either String Value
valueFromTokens isEmpty tokens = do
  (value, rest) <- convert tokens
  if isEmpty rest
    then Right value
    else Left "Trailing garbage"

convert :: Tokens.Tokens k e -> Either e (Value, k)
convert tokens = case tokens of
  Tokens.TkLit lit rest -> Right (litValue lit, rest)
  Tokens.TkText text rest -> Right (StringValue text, rest)
  Tokens.TkNumber number rest -> Right (NumberValue (numberScientific number), rest)
  Tokens.TkArrayOpen items -> convertArray [] items
  Tokens.TkRecordOpen pairs -> convertRecord [] pairs
  Tokens.TkErr err -> Left err

litValue :: Tokens.Lit -> Value
litValue lit = case lit of
  Tokens.LitNull -> NullValue
  Tokens.LitTrue -> BoolValue True
  Tokens.LitFalse -> BoolValue False

numberScientific :: Tokens.Number -> Scientific.Scientific
numberScientific number = case number of
  Tokens.NumInteger n -> fromInteger n
  Tokens.NumDecimal s -> s
  Tokens.NumScientific s -> s

convertArray :: [Value] -> Tokens.TkArray k e -> Either e (Value, k)
convertArray acc items = case items of
  Tokens.TkItem tokens -> do
    (!value, rest) <- convert tokens
    convertArray (value : acc) rest
  Tokens.TkArrayEnd rest -> Right (ArrayValue (Vector.fromList (reverse acc)), rest)
  Tokens.TkArrayErr err -> Left err

convertRecord :: [(Key.Key, Value)] -> Tokens.TkRecord k e -> Either e (Value, k)
convertRecord acc pairs = case pairs of
  Tokens.TkPair key tokens -> do
    (!value, rest) <- convert tokens
    convertRecord ((key, value) : acc) rest
  Tokens.TkRecordEnd rest -> Right (ObjectValue (members acc), rest)
  Tokens.TkRecordErr err -> Left err

members :: [(Key.Key, Value)] -> Members
members reversedPairs =
  let
    pairs = reverse reversedPairs
   in
    Members
      { membersPairs = OMap.assocs (OMap.fromList pairs)
      , membersMap = KeyMap.fromList pairs
      }

isJsonSpace :: Word.Word8 -> Bool
isJsonSpace w =
  w == 0x20 || w == 0x0a || w == 0x0d || w == 0x09

decode :: FC.Schema Decoder a -> LBS.ByteString -> Either String a
decode schema =
  fromValue schema Monad.<=< parseLazy

fromValue :: FC.Schema Decoder a -> Value -> Either String a
fromValue schema =
  let
    Decoder parseValue = FC.schemaInterpreter schema
   in
    AesonTypes.parseEither parseValue

parseLazy :: LBS.ByteString -> Either String Value
parseLazy =
  valueFromTokens (LBS.all isJsonSpace) . DecodingLBS.lbsToTokens

decodeStrict :: FC.Schema Decoder a -> BS.ByteString -> Either String a
decodeStrict schema =
  fromValue schema Monad.<=< parseStrict
