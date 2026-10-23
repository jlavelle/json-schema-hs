module JsonSchema.Validate.Registry
  ( Registry (..)
  , emptyRegistry
  , buildRegistry
  , collectRegistry
  , addSchema
  , insertResource
  , overlay
  , withFormat
  , withFormatAssertion
  , resolveId
  , resolveIdKey
  , Resource
  , resource
  , resourceSchema
  , resourceJson
  , lookupResource
  , lookupResourceSchema
  , ResourceKey
  , resourceUri
  , baseKey
  , resourceKeyUri
  , AnchorKey (..)
  , DefKey (..)
  , DynamicScope (..)
  , emptyScope
  , scopeBaseKey
  , pushResource
  , enterDynamicRef
  , findInnermostResource
  , findOutermostResource
  , lookupAnchorInRegistry
  ) where

import Control.Applicative ((<|>))
import Control.DeepSeq qualified as DeepSeq
import Data.Aeson qualified as Aeson
import Data.Foldable qualified as Foldable
import Data.Map.Ordered qualified as OMap
import Data.Map.Strict qualified as Map
import Data.Maybe qualified as Maybe
import Data.Set qualified as Set
import Data.Text qualified as Text
import GHC.Generics qualified as Generics

import JsonSchema qualified as Schema
import JsonSchema.Validate.Format qualified as Format
import JsonSchema.Validate.Uri qualified as Uri

data Registry = Registry
  { registryById :: !(Map.Map ResourceKey Resource)
  , registryAnchors :: !(Map.Map AnchorKey Schema.Schema)
  , registryDynamicAnchors :: !(Map.Map AnchorKey Schema.Schema)
  , registryDefsByName :: !(Map.Map DefKey Schema.Schema)
  , registryFormats :: !(Map.Map Format.Format Format.FormatPredicate)
  , registryFormatAssertion :: !Format.FormatAssertion
  }
  deriving stock (Generics.Generic)

data ResourceKey
  = DocumentRoot
  | ResourceUri !Text.Text
  deriving stock (Eq, Generics.Generic, Ord, Show)
  deriving anyclass (DeepSeq.NFData)

data Resource = Resource
  { resourceSchema :: !Schema.Schema
  , resourceJson :: Aeson.Value
  }

data AnchorKey = AnchorKey
  { anchorKeyBase :: !ResourceKey
  , anchorKeyName :: !Text.Text
  }
  deriving stock (Eq, Generics.Generic, Ord, Show)
  deriving anyclass (DeepSeq.NFData)

data DefKey = DefKey
  { defKeyName :: !Text.Text
  , defKeyBase :: !ResourceKey
  }
  deriving stock (Eq, Generics.Generic, Ord, Show)
  deriving anyclass (DeepSeq.NFData)

emptyRegistry :: Registry
emptyRegistry = case mempty of
  Generics.Generically registry -> registry

buildRegistry :: Schema.Schema -> Registry
buildRegistry = collectRegistry Nothing

collectRegistry :: Maybe Text.Text -> Schema.Schema -> Registry
collectRegistry = collectRegistryGo True

collectRegistryGo :: Bool -> Maybe Text.Text -> Schema.Schema -> Registry
collectRegistryGo atResourceRoot baseUri schema = case schema of
  Schema.BoolSchema _ -> emptyRegistry
  Schema.ObjectSchema obj ->
    let
      resolvedId = resolveId baseUri =<< Schema.schemaId obj
      currentUri = resolvedId <|> baseUri
      idEntry = case resolvedId of
        Just uri -> emptyRegistry{registryById = Map.singleton (ResourceUri uri) (resource schema)}
        Nothing -> emptyRegistry
      anchorBase = baseKey currentUri
      anchorEntry = case Schema.schemaAnchor obj of
        Just anchor ->
          emptyRegistry
            { registryAnchors =
                Map.singleton (AnchorKey anchorBase (Schema.anchorToText anchor)) schema
            }
        Nothing -> emptyRegistry
      dynamicAnchorEntry = case Schema.schemaDynamicAnchor obj of
        Just dynAnchor ->
          emptyRegistry
            { registryDynamicAnchors =
                Map.singleton (AnchorKey anchorBase (Schema.dynamicAnchorToText dynAnchor)) schema
            }
        Nothing -> emptyRegistry
      isResourceRoot = atResourceRoot || Maybe.isJust resolvedId
      defsEntry = case Schema.schemaDefs obj of
        Just defs
          | isResourceRoot ->
              emptyRegistry
                { registryDefsByName =
                    Foldable.foldl'
                      ( \acc (name, defSchema) ->
                          Map.insert (DefKey name anchorBase) defSchema acc
                      )
                      Map.empty
                      (OMap.assocs defs)
                }
        _ -> emptyRegistry
     in
      idEntry
        `union` anchorEntry
        `union` dynamicAnchorEntry
        `union` defsEntry
        `union` foldr (union . collectRegistryGo False currentUri) emptyRegistry obj

resolveId :: Maybe Text.Text -> Schema.SchemaId -> Maybe Text.Text
resolveId baseUri idVal =
  normalizeUri (Uri.resolveUri baseUri (Schema.schemaIdToText idVal))

normalizeUri :: Text.Text -> Maybe Text.Text
normalizeUri uri = case Uri.stripFragment uri of
  "" -> Nothing
  address -> Just address

resource :: Schema.Schema -> Resource
resource schema = Resource schema (Aeson.toJSON schema)

baseKey :: Maybe Text.Text -> ResourceKey
baseKey = Maybe.fromMaybe DocumentRoot . (>>= resourceUri)

resourceUri :: Text.Text -> Maybe ResourceKey
resourceUri = fmap ResourceUri . normalizeUri

union :: Registry -> Registry -> Registry
union left right = case Generics.Generically left <> Generics.Generically right of
  Generics.Generically registry -> registry

-- | Registers every resource the schema identifies with @$id@. A registered resource replaces any existing resource with the same URI.
addSchema :: Schema.Schema -> Registry -> Registry
addSchema schema = overlay (dropResource DocumentRoot (buildRegistry schema))

overlay :: Registry -> Registry -> Registry
overlay new registry =
  new `union` foldr removeResource registry (Map.keys (registryById new))

removeResource :: ResourceKey -> Registry -> Registry
removeResource key registry
  | Map.member key (registryById registry) = dropResource key registry
  | otherwise = registry

dropResource :: ResourceKey -> Registry -> Registry
dropResource key registry =
  let
    dropBase :: (k -> ResourceKey) -> Map.Map k Schema.Schema -> Map.Map k Schema.Schema
    dropBase base = Map.filterWithKey (\k _ -> base k /= key)
   in
    registry
      { registryById = Map.delete key (registryById registry)
      , registryAnchors = dropBase anchorKeyBase (registryAnchors registry)
      , registryDynamicAnchors = dropBase anchorKeyBase (registryDynamicAnchors registry)
      , registryDefsByName = dropBase defKeyBase (registryDefsByName registry)
      }

-- | Registers the schema under the given URI and under any @$id@ inside it. A registered resource replaces any existing resource with the same URI. The URI's fragment is dropped, and an empty or fragment-only URI behaves like 'addSchema'.
insertResource :: Text.Text -> Schema.Schema -> Registry -> Registry
insertResource uri schema = case resourceUri uri of
  Just key ->
    overlay
      (emptyRegistry{registryById = Map.singleton key (resource schema)} `union` collectRegistry (resourceKeyUri key) schema)
  Nothing -> addSchema schema

resourceKeyUri :: ResourceKey -> Maybe Text.Text
resourceKeyUri key = case key of
  DocumentRoot -> Nothing
  ResourceUri uri -> Just uri

-- | A registered format always asserts, whatever the 'Format.FormatAssertion' setting or the metaschema's vocabularies. Registering a builtin format's name makes that format assert.
withFormat :: Format.Format -> Format.FormatPredicate -> Registry -> Registry
withFormat fmt predicate registry =
  registry{registryFormats = Map.insert fmt predicate (registryFormats registry)}

-- | Governs builtin formats only. Once 'Format.FormatAssertionAlways' is set, setting 'Format.FormatAssertionByVocabulary' has no effect.
withFormatAssertion :: Format.FormatAssertion -> Registry -> Registry
withFormatAssertion assertion registry =
  registry{registryFormatAssertion = assertion <> registryFormatAssertion registry}

resolveIdKey :: Maybe Text.Text -> Schema.SchemaId -> Maybe ResourceKey
resolveIdKey baseUri = fmap ResourceUri . resolveId baseUri

lookupResource :: ResourceKey -> Registry -> Maybe Resource
lookupResource key = Map.lookup key . registryById

lookupResourceSchema :: ResourceKey -> Registry -> Maybe Schema.Schema
lookupResourceSchema key = fmap resourceSchema . lookupResource key

data DynamicScope = DynamicScope
  { scopeResourceStack :: [ResourceKey]
  , scopeActiveDynamicRefs :: Set.Set (Int, ResourceKey, Text.Text)
  }

emptyScope :: DynamicScope
emptyScope = DynamicScope [] Set.empty

scopeBaseKey :: DynamicScope -> ResourceKey
scopeBaseKey scope = case scopeResourceStack scope of
  key : _ -> key
  [] -> DocumentRoot

pushResource :: ResourceKey -> DynamicScope -> DynamicScope
pushResource uri scope = scope{scopeResourceStack = uri : scopeResourceStack scope}

enterDynamicRef :: (Int, ResourceKey, Text.Text) -> DynamicScope -> Maybe DynamicScope
enterDynamicRef key scope =
  if Set.member key (scopeActiveDynamicRefs scope)
    then Nothing
    else Just scope{scopeActiveDynamicRefs = Set.insert key (scopeActiveDynamicRefs scope)}

findInnermostResource :: (ResourceKey -> Maybe a) -> DynamicScope -> Maybe a
findInnermostResource lookupAt =
  Foldable.asum . fmap lookupAt . scopeResourceStack

findOutermostResource :: (ResourceKey -> Maybe a) -> DynamicScope -> Maybe a
findOutermostResource lookupAt =
  Foldable.asum . fmap lookupAt . reverse . scopeResourceStack

lookupAnchorInRegistry :: Text.Text -> Maybe Text.Text -> Registry -> Maybe Schema.Schema
lookupAnchorInRegistry anchor baseUri registry =
  let
    lookupAtBase base =
      let
        key = AnchorKey base anchor
       in
        Map.lookup key (registryAnchors registry) <|> Map.lookup key (registryDynamicAnchors registry)
   in
    lookupAtBase (baseKey baseUri)
