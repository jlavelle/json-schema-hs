module JsonSchema.Validate.Check
  ( SchemaError (..)
  , renderSchemaError
  , checkSchema
  ) where

import Control.DeepSeq qualified as DeepSeq
import Control.Monad qualified as Monad
import Data.Foldable qualified as Foldable
import Data.Map.Ordered qualified as OMap
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import GHC.Generics qualified as Generics

import JsonSchema qualified as Schema
import JsonSchema.Validate.Metaschema qualified as Metaschema
import JsonSchema.Validate.Node qualified as Node
import JsonSchema.Validate.Registry qualified as Registry
import JsonSchema.Validate.Resolve.Prepass qualified as Prepass

data SchemaError
  = InvalidPatternError !Text.Text !Text.Text
  | UnresolvableReferenceError !Text.Text
  | ReferenceCycleError !Text.Text
  deriving stock (Eq, Generics.Generic, Show)
  deriving anyclass (DeepSeq.NFData)

renderSchemaError :: SchemaError -> Text.Text
renderSchemaError err = case err of
  InvalidPatternError keyword pat -> keyword <> " has a pattern that does not compile: " <> pat
  UnresolvableReferenceError ref -> "reference does not resolve: " <> ref
  ReferenceCycleError ref -> "reference forms a cycle that never reaches an instance: " <> ref

-- | Checks that patterns compile, that references resolve with this library's resolver, and that no reference forms a cycle that never reaches an instance. It does not validate against the metaschema. A @$dynamicRef@ with a plain-name fragment and no URI is accepted whenever any @$dynamicAnchor@ of that name is registered, because its target depends on the dynamic scope.
checkSchema :: Registry.Registry -> Schema.Schema -> Either SchemaError ()
checkSchema external schema =
  let
    baseUri = Prepass.schemaBaseUri schema
    registry = Metaschema.documentRegistry external schema
    context =
      CheckContext
        { checkContextRegistry = registry
        , checkContextRootScope = Prepass.resourceScope baseUri baseUri (Registry.resource schema)
        , checkContextRoot = schema
        , checkContextDynamicAnchorNames =
            Set.fromList . fmap Registry.anchorKeyName . Map.keys . Registry.registryDynamicAnchors $ registry
        }
    initial = CheckState (Set.singleton (Prepass.rootRefKey baseUri)) Set.empty
   in
    Monad.void (cover context (checkContextRootScope context) schema initial)

data CheckContext = CheckContext
  { checkContextRegistry :: Registry.Registry
  , checkContextRootScope :: Prepass.ResourceScope
  , checkContextRoot :: Schema.Schema
  , checkContextDynamicAnchorNames :: Set.Set Text.Text
  }

data CheckState = CheckState
  { checkStateCovered :: Set.Set Text.Text
  , checkStateCycleFree :: Set.Set Text.Text
  }

cover :: CheckContext -> Prepass.ResourceScope -> Schema.Schema -> CheckState -> Either SchemaError CheckState
cover context scope schema state = case schema of
  Schema.BoolSchema _ -> Right state
  Schema.ObjectSchema obj -> do
    checkPatterns obj
    walkObject Foldable.toList (cover context) (coverReference context) scope obj state

checkPatterns :: Schema.SchemaObjectF r -> Either SchemaError ()
checkPatterns obj =
  let
    checkPattern keyword pat = case Node.mkCompiledPattern pat of
      Node.Compiled _ _ -> Right ()
      Node.Uncompilable _ -> Left (InvalidPatternError keyword pat)
   in
    do
      Foldable.traverse_ (checkPattern "pattern") (Schema.schemaPattern obj)
      Foldable.traverse_
        (Foldable.traverse_ (checkPattern "patternProperties" . fst) . OMap.assocs)
        (Schema.schemaPatternProperties obj)

walkObject ::
  (Schema.SchemaObjectF Schema.Schema -> [Schema.Schema]) ->
  (Prepass.ResourceScope -> Schema.Schema -> CheckState -> Either SchemaError CheckState) ->
  (Prepass.ResourceScope -> CheckState -> Reference -> Either SchemaError CheckState) ->
  Prepass.ResourceScope ->
  Schema.SchemaObjectF Schema.Schema ->
  CheckState ->
  Either SchemaError CheckState
walkObject children walkChild walkReference scope obj state = do
  afterChildren <-
    Monad.foldM (\acc child -> walkChild (Prepass.enterResource scope child) child acc) state (children obj)
  Monad.foldM (walkReference scope) afterChildren (objectReferences obj)

data Reference
  = StaticReference !Text.Text
  | DynamicAnchorReference !Text.Text !Text.Text

objectReferences :: Schema.SchemaObjectF r -> [Reference]
objectReferences obj =
  let
    dynamicReference dynRef = case Prepass.parseUriRef dynRef of
      (uriPart, Just anchor)
        | Text.null uriPart && not (Text.isPrefixOf "/" anchor) -> DynamicAnchorReference dynRef anchor
      _ -> StaticReference dynRef
   in
    foldMap (pure . StaticReference . Schema.schemaRefToText) (Schema.schemaRef obj)
      <> foldMap (pure . dynamicReference . Schema.dynamicRefToText) (Schema.schemaDynamicRef obj)

coverReference :: CheckContext -> Prepass.ResourceScope -> CheckState -> Reference -> Either SchemaError CheckState
coverReference context scope state reference =
  let
    ref = referenceText reference
   in
    case referenceTarget context scope ref of
      Just (key, targetScope, target) -> do
        cycleChecked <- visit context Set.empty ref key targetScope target state
        if Set.member key (checkStateCovered cycleChecked)
          then Right cycleChecked
          else
            cover
              context
              targetScope
              target
              cycleChecked{checkStateCovered = Set.insert key (checkStateCovered cycleChecked)}
      Nothing -> case reference of
        DynamicAnchorReference _ anchor
          | Set.member anchor (checkContextDynamicAnchorNames context) -> Right state
        _ -> Left (UnresolvableReferenceError ref)

referenceText :: Reference -> Text.Text
referenceText reference = case reference of
  StaticReference ref -> ref
  DynamicAnchorReference ref _ -> ref

referenceTarget ::
  CheckContext -> Prepass.ResourceScope -> Text.Text -> Maybe (Text.Text, Prepass.ResourceScope, Schema.Schema)
referenceTarget context scope ref = case Prepass.refTarget (checkContextRegistry context) scope ref of
  Prepass.RefToRoot key -> Just (key, checkContextRootScope context, checkContextRoot context)
  Prepass.RefTo key targetScope target -> Just (key, targetScope, target)
  Prepass.RefUnresolvable -> Nothing

visit ::
  CheckContext ->
  Set.Set Text.Text ->
  Text.Text ->
  Text.Text ->
  Prepass.ResourceScope ->
  Schema.Schema ->
  CheckState ->
  Either SchemaError CheckState
visit context stack ref key scope target state
  | Set.member key stack = Left (ReferenceCycleError ref)
  | Set.member key (checkStateCycleFree state) = Right state
  | otherwise = do
      finished <- inPlaceWalk context (Set.insert key stack) scope target state
      Right finished{checkStateCycleFree = Set.insert key (checkStateCycleFree finished)}

inPlaceWalk ::
  CheckContext ->
  Set.Set Text.Text ->
  Prepass.ResourceScope ->
  Schema.Schema ->
  CheckState ->
  Either SchemaError CheckState
inPlaceWalk context stack scope schema state = case schema of
  Schema.BoolSchema _ -> Right state
  Schema.ObjectSchema obj ->
    walkObject Prepass.inPlaceSubschemas (inPlaceWalk context stack) (visitReference context stack) scope obj state

visitReference ::
  CheckContext -> Set.Set Text.Text -> Prepass.ResourceScope -> CheckState -> Reference -> Either SchemaError CheckState
visitReference context stack scope state reference =
  let
    ref = referenceText reference
   in
    case referenceTarget context scope ref of
      Just (key, targetScope, target) -> visit context stack ref key targetScope target state
      Nothing -> Right state
