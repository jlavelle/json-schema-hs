module JsonSchema.Generate
  ( JsonSchemaGen (..)
  , GenResult (..)
  , toJsonSchema
  ) where

import Data.Aeson qualified as Aeson
import Data.Coerce qualified as Coerce
import Data.Foldable qualified as Foldable
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Kind qualified as Kind
import Data.List.NonEmpty qualified as NEL
import Data.Map.Ordered qualified as OMap
import Data.Map.Strict qualified as Map
import Data.Maybe qualified as Maybe
import Data.NonEmptyText qualified as NET
import Data.Proxy (Proxy (Proxy), asProxyTypeOf)
import Data.Scientific qualified as Scientific
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Word (Word16, Word32, Word64, Word8)
import Fleece.Core qualified as FC
import GHC.TypeLits qualified as TypeLits
import JsonSchema qualified as Schema

import JsonSchema.OrderedProperties qualified as OrderedProperties

type JsonSchemaGen :: Kind.Type -> Kind.Type
newtype JsonSchemaGen a = JsonSchemaGen {runJsonSchemaGen :: Set.Set Text.Text -> GenResult}

instance FC.Fleece JsonSchemaGen where
  data Object JsonSchemaGen _object _constructor = ObjectAccum
    { objectProps :: !(OMap.OMap Text.Text (Set.Set Text.Text -> GenResult))
    , objectRequiredNames :: !(Seq.Seq Text.Text)
    , objectAdditionalProps :: !(Maybe (Set.Set Text.Text -> GenResult))
    }

  newtype Field JsonSchemaGen _object _a
    = FieldWrap FieldData

  newtype AdditionalFields JsonSchemaGen _object _a
    = AdditionalFieldsWrap (Set.Set Text.Text -> GenResult)

  newtype UnionMembers JsonSchemaGen _allTypes _handledTypes
    = UnionMembersWrap (Seq.Seq (Set.Set Text.Text -> GenResult))

  newtype TaggedUnionMembers JsonSchemaGen _allTags _handledTags
    = TaggedUnionMembersWrap (Seq.Seq (Set.Set Text.Text -> (Text.Text, GenResult)))

  interpretNumber _name = JsonSchemaGen (const (emptyResult (typeSchema Schema.TypeNumber)))
  interpretText _name = JsonSchemaGen (const (emptyResult (typeSchema Schema.TypeString)))
  interpretBoolean _name = JsonSchemaGen (const (emptyResult (typeSchema Schema.TypeBoolean)))
  interpretNull _name = JsonSchemaGen (const (emptyResult (typeSchema Schema.TypeNull)))

  interpretArray _name schema = JsonSchemaGen $ \seen ->
    let
      GenResult itemSchema defs refs = interpretInner schema seen
     in
      GenResult
        ( Schema.ObjectSchema
            Schema.emptySchema
              { Schema.schemaType = Just (NEL.singleton Schema.TypeArray)
              , Schema.schemaItems = Just itemSchema
              }
        )
        defs
        refs

  interpretNullable _name schema = JsonSchemaGen $ \seen ->
    let
      GenResult innerSchema defs refs = interpretInner schema seen
     in
      GenResult
        ( Schema.ObjectSchema
            Schema.emptySchema
              { Schema.schemaAnyOf =
                  Just
                    ( innerSchema
                        NEL.:| [typeSchema Schema.TypeNull]
                    )
              }
        )
        defs
        refs

  interpretDescribe description =
    constrained (\obj -> obj{Schema.schemaDescription = Just (NET.toText description)})

  interpretFormat formatString =
    constrained (\obj -> obj{Schema.schemaFormat = Just (Text.pack formatString)})

  interpretObjectNamed name (ObjectAccum propFns requiredNames additionalPropFn) =
    named name $ \seen ->
      let
        accum = evalObjectAccum propFns additionalPropFn seen
       in
        GenResult (objectAccumSchema requiredNames accum) (accumDefs accum) (accumRefs accum)

  constructor _f =
    ObjectAccum
      { objectProps = OMap.empty
      , objectRequiredNames = Seq.empty
      , objectAdditionalProps = Nothing
      }

  field (ObjectAccum props req addl) (FieldWrap fd) =
    ObjectAccum
      { objectProps = props OMap.|> (fieldName fd, fieldGen fd)
      , objectRequiredNames = case fieldRequired fd of
          Required -> req Seq.|> fieldName fd
          Optional -> req
      , objectAdditionalProps = addl
      }

  additional (ObjectAccum props req _) (AdditionalFieldsWrap addlFn) =
    ObjectAccum
      { objectProps = props
      , objectRequiredNames = req
      , objectAdditionalProps = Just addlFn
      }

  required name _accessor schema =
    FieldWrap (FieldData (Text.pack name) (interpretInner schema) Required)

  optional name _accessor schema =
    FieldWrap (FieldData (Text.pack name) (interpretInner schema) Optional)

  mapField _f = Coerce.coerce

  additionalFields _accessor schema =
    AdditionalFieldsWrap (interpretInner schema)

  interpretValidateNamed name _uncheck _check schema =
    if name == FC.schemaName schema
      then JsonSchemaGen (interpretInner schema)
      else named name (interpretInner schema)

  interpretValidateAnonymous _uncheck _check schema =
    JsonSchemaGen (interpretInner schema)

  interpretBoundedEnumNamed _name toText =
    let
      values = fmap (Aeson.String . toText) [minBound .. maxBound]
     in
      JsonSchemaGen . const . emptyResult . Schema.ObjectSchema
        $ Schema.emptySchema
          { Schema.schemaType = Just (NEL.singleton Schema.TypeString)
          , Schema.schemaEnum = Just values
          }

  interpretUnionNamed name (UnionMembersWrap memberFns) = named name $ \seen ->
    let
      (schemas, allDefs, allRefs) =
        foldMap
          ( \f ->
              let
                GenResult s d r = f seen
               in
                (Seq.singleton s, d, r)
          )
          memberFns
      unionSchema =
        Schema.ObjectSchema
          $ case NEL.nonEmpty . Foldable.toList $ schemas of
            Just nelSchemas -> Schema.emptySchema{Schema.schemaAnyOf = Just nelSchemas}
            Nothing -> Schema.emptySchema
     in
      GenResult unionSchema allDefs allRefs

  unionMemberWithIndex _index schema =
    UnionMembersWrap (Seq.singleton (interpretInner schema))

  unionCombine (UnionMembersWrap left) (UnionMembersWrap right) =
    UnionMembersWrap (left <> right)

  interpretTaggedUnionNamed name tagProperty (TaggedUnionMembersWrap memberFns) =
    named name $ \seen ->
      let
        tagPropName = Text.pack tagProperty
        addTagProp (tagValue, GenResult memberSchema defs refs) = case memberSchema of
          Schema.ObjectSchema obj ->
            let
              tagConstSchema =
                Schema.ObjectSchema
                  Schema.emptySchema
                    { Schema.schemaConst = Just (Aeson.String tagValue)
                    }
              existingProps = Maybe.fromMaybe OMap.empty (Schema.schemaProperties obj)
              existingReq = Foldable.fold (Schema.schemaRequired obj)
             in
              GenResult
                ( Schema.ObjectSchema
                    obj
                      { Schema.schemaProperties = Just (existingProps OMap.|> (tagPropName, tagConstSchema))
                      , Schema.schemaRequired = Just (Set.insert tagPropName existingReq)
                      }
                )
                defs
                refs
          _ -> GenResult memberSchema defs refs
        (schemas, allDefs, allRefs) =
          foldMap
            ( \f ->
                let
                  GenResult s d r = addTagProp (f seen)
                 in
                  (Seq.singleton s, d, r)
            )
            memberFns
        taggedSchema =
          Schema.ObjectSchema
            $ case NEL.nonEmpty . Foldable.toList $ schemas of
              Just nelSchemas -> Schema.emptySchema{Schema.schemaOneOf = Just nelSchemas}
              Nothing -> Schema.emptySchema
       in
        GenResult taggedSchema allDefs allRefs

  taggedUnionMemberWithTag tag (ObjectAccum propFns req addlPropFn) =
    let
      tagValue = Text.pack (TypeLits.symbolVal tag)
     in
      TaggedUnionMembersWrap . Seq.singleton $ \seen ->
        let
          accum = evalObjectAccum propFns addlPropFn seen
         in
          (tagValue, GenResult (objectAccumSchema req accum) (accumDefs accum) (accumRefs accum))

  taggedUnionCombine (TaggedUnionMembersWrap left) (TaggedUnionMembersWrap right) =
    TaggedUnionMembersWrap (left <> right)

  interpretJsonString _schema =
    JsonSchemaGen (const (emptyResult (typeSchema Schema.TypeString)))

  interpretMinLength len = constrained (\obj -> obj{Schema.schemaMinLength = Just (clampToWord len)})
  interpretMaxLength len = constrained (\obj -> obj{Schema.schemaMaxLength = Just (clampToWord len)})
  interpretMinItems len = constrained (\obj -> obj{Schema.schemaMinItems = Just (clampToWord len)})
  interpretMaxItems len = constrained (\obj -> obj{Schema.schemaMaxItems = Just (clampToWord len)})
  interpretMinimum bound = constrained (\obj -> obj{Schema.schemaMinimum = Just (fromInteger bound)})
  interpretMaximum bound = constrained (\obj -> obj{Schema.schemaMaximum = Just (fromInteger bound)})

  interpretInt _name = boundedNumberFor Nothing (Proxy @Int64)
  interpretInt8 _name = boundedNumberFor (Just "int8") (Proxy @Int8)
  interpretInt16 _name = boundedNumberFor (Just "int16") (Proxy @Int16)
  interpretInt32 _name = boundedNumberFor (Just "int32") (Proxy @Int32)
  interpretInt64 _name = boundedNumberFor (Just "int64") (Proxy @Int64)
  interpretWord _name = boundedNumberFor (Just "word") (Proxy @Word64)
  interpretWord8 _name = boundedNumberFor (Just "word8") (Proxy @Word8)
  interpretWord16 _name = boundedNumberFor (Just "word16") (Proxy @Word16)
  interpretWord32 _name = boundedNumberFor (Just "word32") (Proxy @Word32)
  interpretWord64 _name = boundedNumberFor (Just "word64") (Proxy @Word64)
  interpretDouble _name = formattedNumber "double"
  interpretFloat _name = formattedNumber "float"

instance OrderedProperties.OrderedProperties JsonSchemaGen where
  orderedProperties schema =
    FC.Schema
      (FC.annotateName (FC.schemaName schema) "ordered properties")
      ( JsonSchemaGen $ \seen ->
          let
            GenResult itemSchema defs refs = interpretInner schema seen
           in
            GenResult
              ( Schema.ObjectSchema
                  Schema.emptySchema
                    { Schema.schemaType = Just (NEL.singleton Schema.TypeObject)
                    , Schema.schemaAdditionalProperties = Just itemSchema
                    }
              )
              defs
              refs
      )

data GenResult = GenResult
  { resultSchema :: !Schema.Schema
  , resultDefs :: !(Map.Map Text.Text Schema.Schema)
  , resultRefs :: !(Set.Set Text.Text)
  }

data FieldData = FieldData
  { fieldName :: !Text.Text
  , fieldGen :: !(Set.Set Text.Text -> GenResult)
  , fieldRequired :: !Requiredness
  }

data Requiredness = Required | Optional
  deriving stock (Eq, Show)

data ObjectAccumResult = ObjectAccumResult
  { accumPropSchemas :: !(OMap.OMap Text.Text Schema.Schema)
  , accumAdditionalSchema :: !(Maybe Schema.Schema)
  , accumDefs :: !(Map.Map Text.Text Schema.Schema)
  , accumRefs :: !(Set.Set Text.Text)
  }

emptyResult :: Schema.Schema -> GenResult
emptyResult s = GenResult s Map.empty Set.empty

typeSchema :: Schema.SchemaType -> Schema.Schema
typeSchema ty =
  Schema.ObjectSchema Schema.emptySchema{Schema.schemaType = Just (NEL.singleton ty)}

interpretInner :: FC.Schema JsonSchemaGen a -> Set.Set Text.Text -> GenResult
interpretInner schema seen =
  let
    name = Text.pack (FC.nameToString (FC.schemaName schema))
   in
    if Set.member name seen
      then GenResult (refSchema name) Map.empty (Set.singleton name)
      else runJsonSchemaGen (FC.schemaInterpreter schema) seen

refSchema :: Text.Text -> Schema.Schema
refSchema name =
  let
    pointerToken = Text.replace "/" "~1" . Text.replace "~" "~0" $ name
   in
    Schema.ObjectSchema Schema.emptySchema{Schema.schemaRef = Just (Schema.SchemaRef ("#/$defs/" <> pointerToken))}

named :: FC.Name -> (Set.Set Text.Text -> GenResult) -> JsonSchemaGen a
named name build =
  let
    nameText = Text.pack (FC.nameToString name)
   in
    JsonSchemaGen $ \seen ->
      let
        GenResult namedSchema defs refs = build (Set.insert nameText seen)
       in
        if Set.member nameText refs
          then GenResult (refSchema nameText) (Map.insert nameText namedSchema defs) (Set.delete nameText refs)
          else GenResult namedSchema defs refs

constrained :: (Schema.SchemaObject -> Schema.SchemaObject) -> FC.Schema JsonSchemaGen a -> JsonSchemaGen a
constrained constrain schema = JsonSchemaGen $ \seen ->
  let
    GenResult innerSchema defs refs = interpretInner schema seen
    innerObject = case innerSchema of
      Schema.ObjectSchema obj -> obj
      Schema.BoolSchema _ -> Schema.emptySchema{Schema.schemaAllOf = Just (NEL.singleton innerSchema)}
   in
    GenResult (Schema.ObjectSchema (constrain innerObject)) defs refs

evalObjectAccum ::
  OMap.OMap Text.Text (Set.Set Text.Text -> GenResult) ->
  Maybe (Set.Set Text.Text -> GenResult) ->
  Set.Set Text.Text ->
  ObjectAccumResult
evalObjectAccum propFns additionalPropFn seen =
  let
    propResults = fmap (\f -> f seen) propFns
    propSchemas = fmap resultSchema propResults
    (propDefs, propRefs) =
      foldMap (\r -> (resultDefs r, resultRefs r)) propResults
    addlResult = fmap (\f -> f seen) additionalPropFn
    addlSchema = fmap resultSchema addlResult
    addlDefs = foldMap resultDefs addlResult
    addlRefs = foldMap resultRefs addlResult
   in
    ObjectAccumResult
      { accumPropSchemas = propSchemas
      , accumAdditionalSchema = addlSchema
      , accumDefs = propDefs <> addlDefs
      , accumRefs = propRefs <> addlRefs
      }

objectAccumSchema :: Seq.Seq Text.Text -> ObjectAccumResult -> Schema.Schema
objectAccumSchema requiredNames accum =
  Schema.ObjectSchema
    Schema.emptySchema
      { Schema.schemaType = Just (NEL.singleton Schema.TypeObject)
      , Schema.schemaProperties =
          if OMap.null (accumPropSchemas accum) then Nothing else Just (accumPropSchemas accum)
      , Schema.schemaRequired =
          if Seq.null requiredNames
            then Nothing
            else Just (foldr Set.insert Set.empty requiredNames)
      , Schema.schemaAdditionalProperties = accumAdditionalSchema accum
      }

clampToWord :: Integer -> Word
clampToWord = fromInteger . max 0 . min (toInteger (maxBound :: Word))

boundedNumberFor ::
  (Bounded n, Integral n) =>
  Maybe Text.Text ->
  Proxy n ->
  JsonSchemaGen a
boundedNumberFor fmt p =
  boundedNumber fmt (asScientific (asProxyTypeOf minBound p)) (asScientific (asProxyTypeOf maxBound p))
  where
    asScientific = fromInteger . toInteger

boundedNumber :: Maybe Text.Text -> Scientific.Scientific -> Scientific.Scientific -> JsonSchemaGen a
boundedNumber fmt lo hi =
  JsonSchemaGen . const . emptyResult . Schema.ObjectSchema
    $ Schema.emptySchema
      { Schema.schemaType = Just (NEL.singleton Schema.TypeInteger)
      , Schema.schemaMinimum = Just lo
      , Schema.schemaMaximum = Just hi
      , Schema.schemaFormat = fmt
      }

formattedNumber :: Text.Text -> JsonSchemaGen a
formattedNumber fmt =
  JsonSchemaGen . const . emptyResult . Schema.ObjectSchema
    $ Schema.emptySchema
      { Schema.schemaType = Just (NEL.singleton Schema.TypeNumber)
      , Schema.schemaFormat = Just fmt
      }

toJsonSchema :: FC.Schema JsonSchemaGen a -> Schema.Schema
toJsonSchema schema =
  let
    GenResult resultSchema defs _refs =
      runJsonSchemaGen (FC.schemaInterpreter schema) Set.empty
    orderedDefs = Map.foldlWithKey' (\acc k v -> acc OMap.|> (k, v)) OMap.empty defs
   in
    if Map.null defs
      then resultSchema
      else case resultSchema of
        Schema.ObjectSchema obj ->
          Schema.ObjectSchema
            obj{Schema.schemaDefs = Just orderedDefs}
        other ->
          Schema.ObjectSchema
            Schema.emptySchema
              { Schema.schemaDefs = Just orderedDefs
              , Schema.schemaAllOf = Just (NEL.singleton other)
              }
