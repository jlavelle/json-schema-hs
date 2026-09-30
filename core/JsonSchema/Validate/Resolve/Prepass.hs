module JsonSchema.Validate.Resolve.Prepass
  ( Resolved (..)
  , RefKeyword (..)
  , refKeywordText
  , resolvedToSchema
  , resolveResource
  , dereferenceResource
  , resolveInResource
  , ResourceScope (..)
  , resourceScope
  , rootRefKey
  , enterResource
  , mapSubschemas
  , inPlaceSubschemas
  , RefTarget (..)
  , refTarget
  , resolveUriRef
  , ResolvedRefTarget (..)
  , resolvePointerFragment
  , followJsonPointer
  , readMaybeInt
  , unescapeJsonPointerSegment
  , collectDefs
  , lookupDefInSchema
  , schemaBaseUri
  , SimpleRefTarget (..)
  , isDynamicAnchorRef
  , parseSimpleRef
  , parseUriRef
  , tryParseDefsPointer
  , percentDecode
  ) where

import Control.Applicative ((<|>))
import Control.Monad qualified as Monad
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types qualified as AesonTypes
import Data.ByteString qualified as BS
import Data.Char qualified as Char
import Data.Foldable qualified as Foldable
import Data.List.NonEmpty qualified as NEL
import Data.Map.Ordered qualified as OMap
import Data.Map.Strict qualified as Map
import Data.Maybe qualified as Maybe
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TE
import Data.Text.Read qualified as TextRead
import Data.Vector qualified as Vector
import Data.Word qualified as Word

import JsonSchema qualified as Schema
import JsonSchema.Validate.Registry qualified as Registry
import JsonSchema.Validate.Uri qualified as Uri

data Resolved
  = ResolvedBool !Bool
  | ResolvedObject !(Schema.SchemaObjectF Resolved)
  | ResolvedRef !RefKeyword !(Schema.SchemaObjectF Resolved) Resolved
  | ResolvedResource !Registry.ResourceKey Resolved
  | ResolvedRoot
  | ResolvedCycle !RefKeyword !Text.Text
  deriving stock (Eq, Show)

data RefKeyword
  = RefKeyword
  | DynamicRefKeyword
  deriving stock (Eq, Show)

refKeywordText :: RefKeyword -> Text.Text
refKeywordText keyword = case keyword of
  RefKeyword -> "$ref"
  DynamicRefKeyword -> "$dynamicRef"

resolvedToSchema :: Resolved -> Schema.Schema
resolvedToSchema resolved = case resolved of
  ResolvedBool b -> Schema.BoolSchema b
  ResolvedObject obj -> Schema.ObjectSchema (fmap resolvedToSchema obj)
  ResolvedRef keyword obj ResolvedRoot -> Schema.ObjectSchema (withRootRef keyword (fmap resolvedToSchema obj))
  ResolvedRef _ obj target
    | Monad.void obj == Schema.emptySchemaF -> resolvedToSchema target
    | otherwise ->
        let
          siblings = fmap resolvedToSchema obj
         in
          Schema.ObjectSchema
            siblings{Schema.schemaAllOf = Just (NEL.singleton (resolvedToSchema target)) <> Schema.schemaAllOf siblings}
  ResolvedResource _ inner -> resolvedToSchema inner
  ResolvedRoot -> Schema.ObjectSchema (withRootRef RefKeyword Schema.emptySchemaF)
  ResolvedCycle keyword ref -> Schema.ObjectSchema (withRef keyword ref Schema.emptySchemaF)

withRootRef :: RefKeyword -> Schema.SchemaObjectF r -> Schema.SchemaObjectF r
withRootRef keyword = withRef keyword "#"

withRef :: RefKeyword -> Text.Text -> Schema.SchemaObjectF r -> Schema.SchemaObjectF r
withRef keyword ref obj = case keyword of
  RefKeyword -> obj{Schema.schemaRef = Just (Schema.SchemaRef ref)}
  DynamicRefKeyword -> obj{Schema.schemaDynamicRef = Just (Schema.DynamicRef ref)}

resolveResource :: Registry.Registry -> Maybe Text.Text -> Registry.Resource -> Resolved
resolveResource = resolveResourceWith InlineAll

data Inlining
  = InlineAll
  | InlineAcyclic !(Set.Set Text.Text)

resolveResourceWith :: Inlining -> Registry.Registry -> Maybe Text.Text -> Registry.Resource -> Resolved
resolveResourceWith inlining registry baseUri root =
  resolveRefsGo
    registry
    inlining
    (resourceScope baseUri baseUri root)
    (Set.singleton (rootRefKey baseUri))
    (Registry.resourceSchema root)

resolveRefsGo :: Registry.Registry -> Inlining -> ResourceScope -> Set.Set Text.Text -> Schema.Schema -> Resolved
resolveRefsGo registry inlining scope visited schema = case schema of
  Schema.BoolSchema b -> ResolvedBool b
  Schema.ObjectSchema obj ->
    let
      descend childVisited child = resolveRefsGo registry inlining (enterResource scope child) childVisited child
      resolvedObj = mapSubschemas (descend visited) (descend Set.empty) obj
      staticRef keyword siblings =
        resolveStaticRef registry inlining scope visited keyword siblings (ResolvedObject resolvedObj)
     in
      case (Schema.schemaRef resolvedObj, Schema.schemaDynamicRef resolvedObj) of
        (Just ref, _) ->
          staticRef RefKeyword resolvedObj{Schema.schemaRef = Nothing} (Schema.schemaRefToText ref)
        (Nothing, Just dynRef)
          | not (isDynamicAnchorRef (Schema.dynamicRefToText dynRef)) ->
              staticRef DynamicRefKeyword resolvedObj{Schema.schemaDynamicRef = Nothing} (Schema.dynamicRefToText dynRef)
        _ -> ResolvedObject resolvedObj

data ResourceScope = ResourceScope
  { resourceScopeDefs :: OMap.OMap Text.Text Schema.Schema
  , resourceScopeHomeBase :: Maybe Text.Text
  , resourceScopeBaseUri :: Maybe Text.Text
  , resourceScopeRoot :: Aeson.Value
  }

enterResource :: ResourceScope -> Schema.Schema -> ResourceScope
enterResource scope schema = case schema of
  Schema.ObjectSchema obj
    | Just idVal <- Schema.schemaId obj ->
        resourceScope
          (resourceScopeHomeBase scope)
          (Registry.resolveId (resourceScopeBaseUri scope) idVal)
          (Registry.resource schema)
  _ -> scope

resourceScope :: Maybe Text.Text -> Maybe Text.Text -> Registry.Resource -> ResourceScope
resourceScope homeBase baseUri root =
  ResourceScope
    { resourceScopeDefs = collectDefs (Registry.resourceSchema root)
    , resourceScopeHomeBase = homeBase
    , resourceScopeBaseUri = baseUri
    , resourceScopeRoot = Registry.resourceJson root
    }

collectDefs :: Schema.Schema -> OMap.OMap Text.Text Schema.Schema
collectDefs schema = case schema of
  Schema.ObjectSchema obj -> Maybe.fromMaybe OMap.empty (Schema.schemaDefs obj)
  Schema.BoolSchema _ -> OMap.empty

mapSubschemas :: (a -> b) -> (a -> b) -> Schema.SchemaObjectF a -> Schema.SchemaObjectF b
mapSubschemas inPlace moving obj =
  (fmap moving obj)
    { Schema.schemaAllOf = fmap (fmap inPlace) (Schema.schemaAllOf obj)
    , Schema.schemaAnyOf = fmap (fmap inPlace) (Schema.schemaAnyOf obj)
    , Schema.schemaOneOf = fmap (fmap inPlace) (Schema.schemaOneOf obj)
    , Schema.schemaNot = fmap inPlace (Schema.schemaNot obj)
    , Schema.schemaIf = fmap inPlace (Schema.schemaIf obj)
    , Schema.schemaThen = fmap inPlace (Schema.schemaThen obj)
    , Schema.schemaElse = fmap inPlace (Schema.schemaElse obj)
    , Schema.schemaDependentSchemas = fmap (fmap inPlace) (Schema.schemaDependentSchemas obj)
    , Schema.schemaDependencies = fmap (fmap (fmap inPlace)) (Schema.schemaDependencies obj)
    }

resolveStaticRef ::
  Registry.Registry ->
  Inlining ->
  ResourceScope ->
  Set.Set Text.Text ->
  RefKeyword ->
  Schema.SchemaObjectF Resolved ->
  Resolved ->
  Text.Text ->
  Resolved
resolveStaticRef registry inlining scope visited keyword siblings unresolved ref =
  case refTarget registry scope ref of
    RefToRoot key
      | Set.member key visited -> ResolvedRef keyword siblings (ResolvedCycle keyword ref)
      | otherwise -> ResolvedRef keyword siblings ResolvedRoot
    RefTo key targetScope targetSchema ->
      let
        followed = followRef registry inlining targetScope visited keyword ref key targetSchema
       in
        ResolvedRef keyword siblings $ case resourceScopeBaseUri targetScope of
          Just uri
            | resourceScopeBaseUri targetScope /= resourceScopeBaseUri scope
            , Just resourceKey <- Registry.resourceUri uri ->
                ResolvedResource resourceKey followed
          _ -> followed
    RefUnresolvable -> unresolved

data RefTarget
  = RefToRoot !Text.Text
  | RefTo !Text.Text !ResourceScope !Schema.Schema
  | RefUnresolvable

refTarget :: Registry.Registry -> ResourceScope -> Text.Text -> RefTarget
refTarget registry scope rawRef =
  let
    ref = rewriteForeignRootRef scope rawRef
    baseUri = resourceScopeBaseUri scope
    refIn targetScope key target = RefTo key (enterResource targetScope target) target
    local fragment = maybe RefUnresolvable (refIn scope (refKey baseUri fragment))
   in
    if ref == "#"
      then RefToRoot (refKey (resourceScopeHomeBase scope) Text.empty)
      else case parseSimpleRef ref of
        Just (SimpleRefDef name) ->
          maybe RefUnresolvable (refIn scope (defRefKey scope name))
            . OMap.lookup name
            . resourceScopeDefs
            $ scope
        Just (SimpleRefAnchor anchor) ->
          local anchor $ Registry.lookupAnchorInRegistry anchor baseUri registry
        Just (SimpleRefJsonPointer segments) ->
          local (foldMap ("/" <>) segments) $ resolveJsonPointer segments (resourceScopeRoot scope)
        Nothing ->
          case resolveUriRef registry baseUri ref of
            Just target ->
              let
                fragment = snd (parseUriRef ref)
                canonicalUri = resolvedRefTargetCanonicalUri target
                rootScope =
                  resourceScope (resourceScopeHomeBase scope) (Just canonicalUri) (resolvedRefTargetResourceRoot target)
                key = refKey (Just canonicalUri) (Maybe.fromMaybe Text.empty fragment)
               in
                case fragment of
                  Nothing -> RefTo key rootScope (resolvedRefTargetSchema target)
                  Just _ -> refIn rootScope key (resolvedRefTargetSchema target)
            Nothing -> RefUnresolvable

data SimpleRefTarget
  = SimpleRefDef Text.Text
  | SimpleRefAnchor Text.Text
  | SimpleRefJsonPointer [Text.Text]
  deriving stock (Eq, Show)

data ResolvedRefTarget = ResolvedRefTarget
  { resolvedRefTargetResourceRoot :: Registry.Resource
  , resolvedRefTargetCanonicalUri :: Text.Text
  , resolvedRefTargetSchema :: Schema.Schema
  }

rewriteForeignRootRef :: ResourceScope -> Text.Text -> Text.Text
rewriteForeignRootRef scope ref =
  case ref of
    "#"
      | resourceScopeBaseUri scope /= resourceScopeHomeBase scope
      , Just uri <- resourceScopeBaseUri scope ->
          uri
    _ -> ref

refKey :: Maybe Text.Text -> Text.Text -> Text.Text
refKey baseUri fragment = Maybe.fromMaybe Text.empty baseUri <> "#" <> canonicalFragment fragment

canonicalFragment :: Text.Text -> Text.Text
canonicalFragment fragment = case Text.stripPrefix "/" fragment of
  Just pointer -> foldMap (("/" <>) . escapeJsonPointerSegment . unescapeJsonPointerSegment) (Text.splitOn "/" pointer)
  Nothing -> fragment

escapeJsonPointerSegment :: Text.Text -> Text.Text
escapeJsonPointerSegment =
  Text.replace "/" "~1"
    . Text.replace "~" "~0"

unescapeJsonPointerSegment :: Text.Text -> Text.Text
unescapeJsonPointerSegment =
  Text.replace "~0" "~"
    . Text.replace "~1" "/"

parseSimpleRef :: Text.Text -> Maybe SimpleRefTarget
parseSimpleRef ref = case Text.uncons ref of
  Just ('#', rest)
    | not (Text.null rest) -> Just (classifyFragment (percentDecode rest))
  _ -> Nothing

classifyFragment :: Text.Text -> SimpleRefTarget
classifyFragment decoded
  | Text.isPrefixOf "/" decoded =
      let
        segments = Text.splitOn "/" (Text.drop 1 decoded)
       in
        maybe (SimpleRefJsonPointer segments) SimpleRefDef (defsPointerName segments)
  | otherwise = SimpleRefAnchor decoded

defsPointerName :: [Text.Text] -> Maybe Text.Text
defsPointerName segments = case segments of
  [defsKeyword, name] | defsKeyword == "$defs" -> Just (unescapeJsonPointerSegment name)
  _ -> Nothing

percentDecode :: Text.Text -> Text.Text
percentDecode input = Text.concat (go input)
  where
    go t = case Text.uncons t of
      Nothing -> []
      Just ('%', _) ->
        case collectTriplets t of
          (bytes@(_ : _), remaining)
            | Right decoded <- TE.decodeUtf8' (BS.pack bytes) -> decoded : go remaining
          _ -> Text.singleton '%' : go (Text.drop 1 t)
      Just (c, rest) -> Text.singleton c : go rest

collectTriplets :: Text.Text -> ([Word.Word8], Text.Text)
collectTriplets t = case Text.uncons t of
  Just ('%', rest) -> case hexTriplet rest of
    Just (byte, rest') ->
      let
        (bytes, remaining) = collectTriplets rest'
       in
        (byte : bytes, remaining)
    Nothing -> ([], t)
  _ -> ([], t)

hexTriplet :: Text.Text -> Maybe (Word.Word8, Text.Text)
hexTriplet t = case Text.uncons t of
  Just (hi, rest1) -> case Text.uncons rest1 of
    Just (lo, rest2) -> do
      high <- hexDigitValue hi
      low <- hexDigitValue lo
      pure (high * 16 + low, rest2)
    Nothing -> Nothing
  Nothing -> Nothing

hexDigitValue :: Char -> Maybe Word.Word8
hexDigitValue c
  | Char.isHexDigit c = Just (fromIntegral (Char.digitToInt c))
  | otherwise = Nothing

defRefKey :: ResourceScope -> Text.Text -> Text.Text
defRefKey scope name = refKey (resourceScopeBaseUri scope) ("/$defs/" <> escapeJsonPointerSegment name)

resolveJsonPointer :: [Text.Text] -> Aeson.Value -> Maybe Schema.Schema
resolveJsonPointer segments root = do
  targetValue <- followJsonPointer (fmap unescapeJsonPointerSegment segments) root
  AesonTypes.parseMaybe Aeson.parseJSON targetValue

followJsonPointer :: [Text.Text] -> Aeson.Value -> Maybe Aeson.Value
followJsonPointer = foldr step Just
  where
    step seg k val = case val of
      Aeson.Object obj -> KeyMap.lookup (Key.fromText seg) obj >>= k
      Aeson.Array arr -> readMaybeInt seg >>= (arr Vector.!?) >>= k
      _ -> Nothing

readMaybeInt :: Text.Text -> Maybe Int
readMaybeInt t = case TextRead.decimal t of
  Right (n, rest)
    | Text.null rest
    , t == "0" || not (Text.isPrefixOf "0" t)
    , n <= toInteger (maxBound :: Int) ->
        Just (fromInteger n)
  _ -> Nothing

resolveUriRef :: Registry.Registry -> Maybe Text.Text -> Text.Text -> Maybe ResolvedRefTarget
resolveUriRef registry baseUri ref =
  let
    (uriPart, fragment) = parseUriRef ref
   in
    if Text.null uriPart
      then Nothing
      else
        let
          resolvedUri = Uri.resolveUri baseUri uriPart
          lookupById uri = do
            key <- Registry.resourceUri uri
            resourceRoot <- Registry.lookupResource key registry
            pure (uri, key, resourceRoot)
          found = lookupById resolvedUri <|> lookupById uriPart
         in
          case found of
            Nothing -> Nothing
            Just (canonicalUri, resourceKey, resourceRoot) ->
              let
                foundUnder = ResolvedRefTarget resourceRoot canonicalUri
                rootSchema = Registry.resourceSchema resourceRoot
               in
                case fragment of
                  Nothing -> Just (foundUnder rootSchema)
                  Just frag -> case tryParseDefsPointer frag of
                    Just name -> fmap foundUnder (lookupDefInSchema name rootSchema)
                    Nothing
                      | Text.isPrefixOf "/" frag ->
                          fmap foundUnder (resolvePointerFragment frag (Registry.resourceJson resourceRoot))
                    Nothing ->
                      let
                        key = Registry.AnchorKey resourceKey frag
                       in
                        fmap foundUnder
                          $ Map.lookup key (Registry.registryAnchors registry)
                            <|> Map.lookup key (Registry.registryDynamicAnchors registry)

parseUriRef :: Text.Text -> (Text.Text, Maybe Text.Text)
parseUriRef ref =
  let
    (uriPart, fragmentPart) = Text.breakOn "#" ref
   in
    case Text.uncons fragmentPart of
      Just ('#', frag) | not (Text.null frag) -> (uriPart, Just (percentDecode frag))
      _ -> (uriPart, Nothing)

tryParseDefsPointer :: Text.Text -> Maybe Text.Text
tryParseDefsPointer fragment =
  let
    body = if Text.isPrefixOf "/" fragment then Text.drop 1 fragment else fragment
   in
    defsPointerName (Text.splitOn "/" body)

lookupDefInSchema :: Text.Text -> Schema.Schema -> Maybe Schema.Schema
lookupDefInSchema name schema = case schema of
  Schema.ObjectSchema obj -> Schema.schemaDefs obj >>= OMap.lookup name
  _ -> Nothing

resolvePointerFragment :: Text.Text -> Aeson.Value -> Maybe Schema.Schema
resolvePointerFragment fragment = resolveJsonPointer (Text.splitOn "/" (Text.drop 1 fragment))

followRef ::
  Registry.Registry ->
  Inlining ->
  ResourceScope ->
  Set.Set Text.Text ->
  RefKeyword ->
  Text.Text ->
  Text.Text ->
  Schema.Schema ->
  Resolved
followRef registry inlining scope visited keyword ref key target = case inlining of
  _ | Set.member key visited -> ResolvedCycle keyword ref
  InlineAcyclic stack
    | Set.member key stack -> ResolvedObject Schema.emptySchemaF{Schema.schemaRef = Just (Schema.SchemaRef key)}
    | otherwise -> resolveRefsGo registry (InlineAcyclic (Set.insert key stack)) scope (Set.insert key visited) target
  InlineAll -> resolveRefsGo registry inlining scope (Set.insert key visited) target

isDynamicAnchorRef :: Text.Text -> Bool
isDynamicAnchorRef dynRef = case snd (parseUriRef dynRef) of
  Just fragment -> not (Text.isPrefixOf "/" fragment)
  Nothing -> False

rootRefKey :: Maybe Text.Text -> Text.Text
rootRefKey baseUri = refKey baseUri Text.empty

dereferenceResource :: Registry.Registry -> Maybe Text.Text -> Registry.Resource -> Resolved
dereferenceResource = resolveResourceWith (InlineAcyclic Set.empty)

resolveInResource :: Registry.Registry -> Maybe Text.Text -> Registry.Resource -> Schema.Schema -> Resolved
resolveInResource registry baseUri resourceRoot target =
  resolveRefsGo registry InlineAll (enterResource (resourceScope baseUri baseUri resourceRoot) target) Set.empty target

inPlaceSubschemas :: Schema.SchemaObjectF a -> [a]
inPlaceSubschemas obj =
  foldMap NEL.toList (Schema.schemaAllOf obj)
    <> foldMap NEL.toList (Schema.schemaAnyOf obj)
    <> foldMap NEL.toList (Schema.schemaOneOf obj)
    <> Foldable.toList (Schema.schemaNot obj)
    <> Foldable.toList (Schema.schemaIf obj)
    <> Foldable.toList (Schema.schemaThen obj)
    <> Foldable.toList (Schema.schemaElse obj)
    <> foldMap Foldable.toList (Schema.schemaDependentSchemas obj)
    <> foldMap (foldMap Foldable.toList) (Schema.schemaDependencies obj)

schemaBaseUri :: Schema.Schema -> Maybe Text.Text
schemaBaseUri schema = case schema of
  Schema.ObjectSchema obj -> Registry.resolveId Nothing =<< Schema.schemaId obj
  _ -> Nothing
