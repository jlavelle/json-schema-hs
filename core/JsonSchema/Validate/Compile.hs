module JsonSchema.Validate.Compile
  ( compile
  , validate
  , validateOn
  , isValid
  , isValidOn
  , dereference
  ) where

import Control.Applicative ((<|>))
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Foldable qualified as Foldable
import Data.Map.Lazy qualified as MapLazy
import Data.Map.Ordered qualified as OMap
import Data.Map.Strict qualified as Map
import Data.Maybe qualified as Maybe
import Data.Text qualified as Text

import JsonSchema qualified as Schema
import JsonSchema.Validate.Dialect qualified as Dialect
import JsonSchema.Validate.Error qualified as Error
import JsonSchema.Validate.Evaluation qualified as Evaluation
import JsonSchema.Validate.Format qualified as Format
import JsonSchema.Validate.Metaschema qualified as Metaschema
import JsonSchema.Validate.Node qualified as Node
import JsonSchema.Validate.Numeric qualified as Numeric
import JsonSchema.Validate.Object qualified as Object
import JsonSchema.Validate.Path qualified as Path
import JsonSchema.Validate.Registry qualified as Registry
import JsonSchema.Validate.Resolve.Prepass qualified as Prepass
import JsonSchema.Validate.Uri qualified as Uri

compile :: Registry.Registry -> Schema.Schema -> Node.Validator
compile externalRegistry schema =
  let
    rootKey = Registry.baseKey (Prepass.schemaBaseUri schema)
    rootResource = Registry.resource schema
    documentRegistry = Metaschema.documentRegistry externalRegistry schema
    fullRegistry =
      documentRegistry
        { Registry.registryById =
            Map.insertWith (\_ existing -> existing) rootKey rootResource (Registry.registryById documentRegistry)
        }
    targetCache = buildTargetCache fullRegistry
    rootValidator =
      Maybe.fromMaybe
        (compileResourceRoot fullRegistry targetCache rootKey rootResource)
        (Map.lookup (ResourceTarget rootKey) targetCache)
   in
    Node.Validator
      { Node.validatorRoot =
          Node.NodeValidator $ \ip sp scope -> Node.runNodeValidator rootValidator ip sp (Registry.pushResource rootKey scope)
      }

data TargetKey
  = ResourceTarget !Registry.ResourceKey
  | AnchorTarget !Registry.AnchorKey
  | DynamicAnchorTarget !Registry.AnchorKey
  | DefTarget !Registry.DefKey
  deriving stock (Eq, Ord)

buildTargetCache :: Registry.Registry -> TargetCache
buildTargetCache registry =
  let
    resourceTargets =
      MapLazy.mapKeysMonotonic ResourceTarget
        . MapLazy.mapWithKey (compileResourceRoot registry targetCache)
        $ Registry.registryById registry
    schemaTargets =
      MapLazy.unions
        [ MapLazy.mapKeysMonotonic AnchorTarget (Registry.registryAnchors registry)
        , MapLazy.mapKeysMonotonic DynamicAnchorTarget (Registry.registryDynamicAnchors registry)
        , MapLazy.mapKeysMonotonic DefTarget (Registry.registryDefsByName registry)
        ]
    compileTarget key = targetValidator registry targetCache (Registry.resourceKeyUri (targetKeyBase key))
    targetCache = MapLazy.union resourceTargets (MapLazy.mapWithKey compileTarget schemaTargets)
   in
    targetCache

type TargetCache = Map.Map TargetKey Node.NodeValidator

compileResourceRoot ::
  Registry.Registry ->
  TargetCache ->
  Registry.ResourceKey ->
  Registry.Resource ->
  Node.NodeValidator
compileResourceRoot registry targetCache key resource =
  let
    resourceRoot =
      compileNode
        Dialect.defaultDialect
        registry
        targetCache
        resourceRoot
        (Prepass.resolveResource registry (Registry.resourceKeyUri key) resource)
   in
    resourceRoot

compileNode ::
  Dialect.Dialect ->
  Registry.Registry ->
  TargetCache ->
  Node.NodeValidator ->
  Prepass.Resolved ->
  Node.NodeValidator
compileNode inheritedDialect registry targetCache rootValidator schema = case schema of
  Prepass.ResolvedBool True -> Node.NodeValidator $ \_ _ _ _ -> Evaluation.emptyResult
  Prepass.ResolvedBool False -> Node.NodeValidator $ \ip sp _ _ -> Evaluation.errorResult $ Error.validationError ip sp Error.FalseSchemaRejection
  Prepass.ResolvedRoot -> rootValidator
  Prepass.ResolvedCycle keyword ref -> Node.NodeValidator $ \ip sp _ _ -> refCycle keyword ip sp ref
  Prepass.ResolvedObject obj -> compileObject inheritedDialect registry targetCache rootValidator obj Nothing
  Prepass.ResolvedRef keyword obj target ->
    compileObject inheritedDialect registry targetCache rootValidator obj (Just (keyword, target))
  Prepass.ResolvedResource key inner ->
    let
      innerValidator = compileNode inheritedDialect registry targetCache rootValidator inner
     in
      Node.NodeValidator $ \ip sp scope -> Node.runNodeValidator innerValidator ip sp (Registry.pushResource key scope)

refCycle :: Prepass.RefKeyword -> Path.InstancePath -> Path.SchemaPath -> Text.Text -> Evaluation.Evaluation
refCycle keyword ip sp ref =
  Evaluation.cycleResult (Error.validationErrorAtPath ip sp (Error.RefCycle (Prepass.refKeywordText keyword) ref))

compileObject ::
  Dialect.Dialect ->
  Registry.Registry ->
  TargetCache ->
  Node.NodeValidator ->
  Schema.SchemaObjectF Prepass.Resolved ->
  Maybe (Prepass.RefKeyword, Prepass.Resolved) ->
  Node.NodeValidator
compileObject inheritedDialect registry targetCache rootValidator obj resolvedRef =
  let
    nodeDialect = objectDialect registry inheritedDialect obj
    assertFormats =
      Registry.registryFormatAssertion registry == Format.FormatAssertionAlways
        || Dialect.dialectAssertFormats nodeDialect
    objV = fmap (compileNode nodeDialect registry targetCache rootValidator) obj
    refV = fmap (fmap (compileNode nodeDialect registry targetCache rootValidator)) resolvedRef
    prefixItemsList = foldMap Foldable.toList . Schema.schemaPrefixItems $ objV
    patternProps =
      fmap (fmap (\(pat, v) -> (Node.mkCompiledPattern pat, v)) . OMap.assocs)
        . Schema.schemaPatternProperties
        $ objV
    propValidators =
      foldMap (KeyMap.fromMap . MapLazy.mapKeysMonotonic Key.fromText . OMap.toMap) . Schema.schemaProperties $ objV
    compiled =
      Node.CompiledObj
        { Node.compiledStringPattern = fmap Node.mkCompiledPattern . Schema.schemaPattern $ objV
        , Node.compiledPatternProperties = patternProps
        , Node.compiledAdditionalPatternMatchers =
            foldMap (Maybe.mapMaybe (Node.compiledPatternMatcher . fst)) patternProps
        , Node.compiledPropValidators = propValidators
        , Node.compiledPropValidatorCount = KeyMap.size propValidators
        , Node.compiledFormat =
            Schema.schemaFormat obj >>= \fmt ->
              (,) fmt <$> Format.assertedFormat (Registry.registryFormats registry) assertFormats fmt
        , Node.compiledMultipleOf =
            Numeric.normalizeScientific . Schema.positiveScientificToScientific
              =<< Schema.schemaMultipleOf objV
        , Node.compiledPrefixItems = prefixItemsList
        , Node.compiledPrefixItemsLength = length prefixItemsList
        , Node.compiledVocabularyFlags = Dialect.dialectVocabularyFlags nodeDialect
        }
   in
    Node.NodeValidator $ \ip sp scope value ->
      let
        currentScope = case Registry.resolveIdKey (scopeBaseUri scope) =<< Schema.schemaId objV of
          Just key -> Registry.pushResource key scope
          Nothing -> scope
       in
        let
          refResult = case (refV, fmap Schema.schemaRefToText (Schema.schemaRef objV)) of
            (Just (keyword, targetV), _) ->
              Node.runNodeValidator targetV ip (sp <> refSegment keyword) currentScope value
            (Nothing, Just ref) ->
              resolveRefAtValidation registry targetCache ip (sp <> refSegment Prepass.RefKeyword) currentScope ref value
            (Nothing, Nothing) -> Evaluation.emptyResult
          dynRefResult =
            foldMap
              ( \dynRef ->
                  resolveDynamicRef
                    registry
                    targetCache
                    ip
                    (sp <> refSegment Prepass.DynamicRefKeyword)
                    currentScope
                    (Schema.dynamicRefToText dynRef)
                    value
              )
              (Schema.schemaDynamicRef objV)
         in
          Object.validateObjectWithApplicators ip sp currentScope objV compiled value (refResult <> dynRefResult)

objectDialect :: Registry.Registry -> Dialect.Dialect -> Schema.SchemaObjectF r -> Dialect.Dialect
objectDialect registry inheritedDialect obj = case Schema.schemaMetaschema obj of
  Just uri -> Dialect.dialectForMetaschema registry (Schema.metaschemaUriToText uri)
  Nothing -> inheritedDialect

scopeBaseUri :: Registry.DynamicScope -> Maybe Text.Text
scopeBaseUri = Registry.resourceKeyUri . Registry.scopeBaseKey

refSegment :: Prepass.RefKeyword -> Path.SchemaPath
refSegment = Path.schemaSegment . Prepass.refKeywordText

resolveRefAtValidation ::
  Registry.Registry ->
  TargetCache ->
  Path.InstancePath ->
  Path.SchemaPath ->
  Registry.DynamicScope ->
  Text.Text ->
  Aeson.Value ->
  Evaluation.Evaluation
resolveRefAtValidation registry targetCache ip sp scope ref value =
  let
    unresolved = refUnresolved Prepass.RefKeyword ip sp ref
   in
    case Prepass.parseSimpleRef ref of
      Just (Prepass.SimpleRefDef _) -> unresolved
      Just (Prepass.SimpleRefAnchor anchor) ->
        case anchorTarget registry (Registry.AnchorKey (Registry.scopeBaseKey scope) anchor) of
          Just (key, targetSchema) -> runTarget registry targetCache ip sp scope key targetSchema value
          Nothing -> unresolved
      Just (Prepass.SimpleRefJsonPointer _) -> unresolved
      Nothing ->
        let
          (uriPart, fragment) = Prepass.parseUriRef ref
          cachedTarget = do
            key <- Registry.resourceUri (Uri.resolveUri (scopeBaseUri scope) uriPart)
            Map.lookup (ResourceTarget key) targetCache
         in
          case (fragment, cachedTarget) of
            (Nothing, Just cached) -> Node.runNodeValidator cached ip sp scope value
            _ -> runResolvedUriRef registry targetCache ip sp scope Prepass.RefKeyword ref value

refUnresolved :: Prepass.RefKeyword -> Path.InstancePath -> Path.SchemaPath -> Text.Text -> Evaluation.Evaluation
refUnresolved keyword ip sp ref =
  Evaluation.errorResult (Error.validationErrorAtPath ip sp (Error.RefUnresolved (Prepass.refKeywordText keyword) ref))

anchorTarget :: Registry.Registry -> Registry.AnchorKey -> Maybe (TargetKey, Schema.Schema)
anchorTarget registry key =
  fmap (AnchorTarget key,) (Map.lookup key (Registry.registryAnchors registry))
    <|> fmap (DynamicAnchorTarget key,) (Map.lookup key (Registry.registryDynamicAnchors registry))

runTarget ::
  Registry.Registry ->
  TargetCache ->
  Path.InstancePath ->
  Path.SchemaPath ->
  Registry.DynamicScope ->
  TargetKey ->
  Schema.Schema ->
  Aeson.Value ->
  Evaluation.Evaluation
runTarget registry targetCache ip sp scope key targetSchema =
  case Map.lookup key targetCache of
    Just cached -> Node.runNodeValidator cached ip sp scope
    Nothing -> runUncachedTarget registry targetCache ip sp scope (Registry.resourceKeyUri (targetKeyBase key)) targetSchema

runUncachedTarget ::
  Registry.Registry ->
  TargetCache ->
  Path.InstancePath ->
  Path.SchemaPath ->
  Registry.DynamicScope ->
  Maybe Text.Text ->
  Schema.Schema ->
  Aeson.Value ->
  Evaluation.Evaluation
runUncachedTarget registry targetCache ip sp scope baseUri targetSchema =
  Node.runNodeValidator (targetValidator registry targetCache baseUri targetSchema) ip sp scope

targetValidator :: Registry.Registry -> TargetCache -> Maybe Text.Text -> Schema.Schema -> Node.NodeValidator
targetValidator registry targetCache seedBase targetSchema =
  let
    resourceKey = Registry.baseKey seedBase
    ownResource = do
      key <- Registry.resourceUri =<< Prepass.schemaBaseUri targetSchema
      Map.lookup (ResourceTarget key) targetCache
    resolvedTarget = case Registry.lookupResource resourceKey registry of
      Just resourceRoot -> Prepass.resolveInResource registry seedBase resourceRoot targetSchema
      Nothing -> Prepass.resolveResource registry (Prepass.schemaBaseUri targetSchema <|> seedBase) (Registry.resource targetSchema)
    compiled =
      compileNode
        (resourceDialect registry resourceKey)
        registry
        targetCache
        (Maybe.fromMaybe compiled (Map.lookup (ResourceTarget resourceKey) targetCache))
        resolvedTarget
   in
    Maybe.fromMaybe compiled ownResource

resourceDialect :: Registry.Registry -> Registry.ResourceKey -> Dialect.Dialect
resourceDialect registry key = case Registry.lookupResourceSchema key registry of
  Just (Schema.ObjectSchema obj) -> objectDialect registry Dialect.defaultDialect obj
  _ -> Dialect.defaultDialect

targetKeyBase :: TargetKey -> Registry.ResourceKey
targetKeyBase key = case key of
  ResourceTarget resourceKey -> resourceKey
  AnchorTarget anchorKey -> Registry.anchorKeyBase anchorKey
  DynamicAnchorTarget anchorKey -> Registry.anchorKeyBase anchorKey
  DefTarget defKey -> Registry.defKeyBase defKey

runResolvedUriRef ::
  Registry.Registry ->
  TargetCache ->
  Path.InstancePath ->
  Path.SchemaPath ->
  Registry.DynamicScope ->
  Prepass.RefKeyword ->
  Text.Text ->
  Aeson.Value ->
  Evaluation.Evaluation
runResolvedUriRef registry targetCache ip sp scope keyword ref value =
  case Prepass.resolveUriRef registry (scopeBaseUri scope) ref of
    Just target ->
      runUncachedTarget
        registry
        targetCache
        ip
        sp
        scope
        (Just (Prepass.resolvedRefTargetCanonicalUri target))
        (Prepass.resolvedRefTargetSchema target)
        value
    Nothing -> refUnresolved keyword ip sp ref

resolveDynamicRef ::
  Registry.Registry ->
  TargetCache ->
  Path.InstancePath ->
  Path.SchemaPath ->
  Registry.DynamicScope ->
  Text.Text ->
  Aeson.Value ->
  Evaluation.Evaluation
resolveDynamicRef registry targetCache ip sp outerScope dynRef value =
  case Registry.enterDynamicRef
    (Path.instancePathDepth ip, Registry.scopeBaseKey outerScope, dynRef)
    outerScope of
    Nothing -> refCycle Prepass.DynamicRefKeyword ip sp dynRef
    Just scope ->
      let
        (uriPart, fragment) = Prepass.parseUriRef dynRef
       in
        case fragment of
          Just pointer
            | Text.isPrefixOf "/" pointer ->
                if Text.null uriPart
                  then resolveJsonPointerRef registry targetCache ip sp scope dynRef pointer value
                  else runResolvedUriRef registry targetCache ip sp scope Prepass.DynamicRefKeyword dynRef value
          _
            | not (Text.null uriPart) ->
                resolveUriDynamicRef
                  registry
                  targetCache
                  ip
                  sp
                  scope
                  dynRef
                  uriPart
                  (Maybe.fromMaybe Text.empty fragment)
                  value
          Just anchor -> resolvePlainDynamicAnchor registry targetCache ip sp scope dynRef anchor value
          Nothing -> refUnresolved Prepass.DynamicRefKeyword ip sp dynRef

resolveJsonPointerRef ::
  Registry.Registry ->
  TargetCache ->
  Path.InstancePath ->
  Path.SchemaPath ->
  Registry.DynamicScope ->
  Text.Text ->
  Text.Text ->
  Aeson.Value ->
  Evaluation.Evaluation
resolveJsonPointerRef registry targetCache ip sp scope origRef pointer value =
  let
    unresolved = refUnresolved Prepass.DynamicRefKeyword ip sp origRef
    baseKey = Registry.scopeBaseKey scope
    resource = Registry.lookupResource baseKey registry
   in
    case Prepass.tryParseDefsPointer pointer of
      Just name ->
        case Prepass.lookupDefInSchema name . Registry.resourceSchema =<< resource of
          Just defSchema ->
            runTarget registry targetCache ip sp scope (DefTarget (Registry.DefKey name baseKey)) defSchema value
          Nothing -> searchDefsInScope registry name targetCache ip sp scope origRef value
      Nothing ->
        case Prepass.resolvePointerFragment pointer . Registry.resourceJson =<< resource of
          Just targetSchema ->
            runUncachedTarget registry targetCache ip sp scope (Registry.resourceKeyUri baseKey) targetSchema value
          Nothing -> unresolved

searchDefsInScope ::
  Registry.Registry ->
  Text.Text ->
  TargetCache ->
  Path.InstancePath ->
  Path.SchemaPath ->
  Registry.DynamicScope ->
  Text.Text ->
  Aeson.Value ->
  Evaluation.Evaluation
searchDefsInScope registry name targetCache ip sp scope origRef value =
  let
    defs = Registry.registryDefsByName registry
    scopedHit =
      Registry.findInnermostResource
        (\uri -> let key = Registry.DefKey name uri in fmap (key,) (Map.lookup key defs))
        scope
   in
    case scopedHit of
      Just (key, targetSchema) -> runTarget registry targetCache ip sp scope (DefTarget key) targetSchema value
      Nothing -> refUnresolved Prepass.DynamicRefKeyword ip sp origRef

resolveUriDynamicRef ::
  Registry.Registry ->
  TargetCache ->
  Path.InstancePath ->
  Path.SchemaPath ->
  Registry.DynamicScope ->
  Text.Text ->
  Text.Text ->
  Text.Text ->
  Aeson.Value ->
  Evaluation.Evaluation
resolveUriDynamicRef registry targetCache ip sp scope origRef uri fragment value =
  let
    unresolved = refUnresolved Prepass.DynamicRefKeyword ip sp origRef
    runAt targetKey targetSchema = runTarget registry targetCache ip sp scope targetKey targetSchema value
    viaOutermost = runOutermostDynamicAnchor registry targetCache ip sp scope fragment
    resolved = do
      resourceKey <- Registry.resourceUri (Uri.resolveUri (scopeBaseUri scope) uri)
      baseSchema <- Registry.lookupResourceSchema resourceKey registry
      pure (resourceKey, Registry.AnchorKey resourceKey fragment, baseSchema)
   in
    case resolved of
      Nothing -> unresolved
      Just (resourceKey, key, baseSchema)
        | Text.null fragment -> runAt (ResourceTarget resourceKey) baseSchema
        | Just dynamicTarget <- Map.lookup key (Registry.registryDynamicAnchors registry) ->
            viaOutermost (runAt (DynamicAnchorTarget key) dynamicTarget) value
        | Just staticTarget <- Map.lookup key (Registry.registryAnchors registry) -> runAt (AnchorTarget key) staticTarget
        | otherwise -> unresolved

runOutermostDynamicAnchor ::
  Registry.Registry ->
  TargetCache ->
  Path.InstancePath ->
  Path.SchemaPath ->
  Registry.DynamicScope ->
  Text.Text ->
  Evaluation.Evaluation ->
  Aeson.Value ->
  Evaluation.Evaluation
runOutermostDynamicAnchor registry targetCache ip sp scope anchor fallback value =
  case findOutermostDynamicAnchor anchor registry scope of
    Just (key, dynSchema) -> runTarget registry targetCache ip sp scope (DynamicAnchorTarget key) dynSchema value
    Nothing -> fallback

findOutermostDynamicAnchor ::
  Text.Text -> Registry.Registry -> Registry.DynamicScope -> Maybe (Registry.AnchorKey, Schema.Schema)
findOutermostDynamicAnchor anchor registry =
  let
    lookupKey uri =
      let
        key = Registry.AnchorKey uri anchor
       in
        fmap (key,) . Map.lookup key . Registry.registryDynamicAnchors $ registry
   in
    Registry.findOutermostResource lookupKey

resolvePlainDynamicAnchor ::
  Registry.Registry ->
  TargetCache ->
  Path.InstancePath ->
  Path.SchemaPath ->
  Registry.DynamicScope ->
  Text.Text ->
  Text.Text ->
  Aeson.Value ->
  Evaluation.Evaluation
resolvePlainDynamicAnchor registry targetCache ip sp scope origRef anchor value =
  let
    viaOutermost = runOutermostDynamicAnchor registry targetCache ip sp scope anchor
    run key targetSchema = runTarget registry targetCache ip sp scope key targetSchema value
   in
    case anchorTarget registry (Registry.AnchorKey (Registry.scopeBaseKey scope) anchor) of
      Just (key, targetSchema)
        | hasDynamicAnchor anchor targetSchema -> viaOutermost (run key targetSchema) value
        | otherwise -> run key targetSchema
      Nothing -> viaOutermost (refUnresolved Prepass.DynamicRefKeyword ip sp origRef) value

hasDynamicAnchor :: Text.Text -> Schema.Schema -> Bool
hasDynamicAnchor anchor schema = case schema of
  Schema.ObjectSchema obj -> fmap Schema.dynamicAnchorToText (Schema.schemaDynamicAnchor obj) == Just anchor
  _ -> False

validate :: Node.Validator -> Aeson.Value -> [Error.ValidationError]
validate validator =
  Evaluation.validationErrors
    . Node.runNodeValidator (Node.validatorRoot validator) mempty mempty Registry.emptyScope

validateOn :: Schema.Schema -> Aeson.Value -> [Error.ValidationError]
validateOn = validate . compile Registry.emptyRegistry

isValid :: Node.Validator -> Aeson.Value -> Bool
isValid validator = null . validate validator

isValidOn :: Schema.Schema -> Aeson.Value -> Bool
isValidOn = isValid . compile Registry.emptyRegistry

-- | Inlines each reference. A recursive reference is inlined once along each path and then left as a @$ref@ to its target's canonical URI.
dereference :: Registry.Registry -> Schema.Schema -> Schema.Schema
dereference externalRegistry schema =
  Prepass.resolvedToSchema
    . Prepass.dereferenceResource (Metaschema.documentRegistry externalRegistry schema) (Prepass.schemaBaseUri schema)
    $ Registry.resource schema
