module ReferenceTests
  ( referenceTests
  ) where

import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.ByteString.Lazy qualified as LBS
import JsonSchema qualified as Schema
import JsonSchema.Validate qualified as Validate
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit qualified as HUnit

import TestUtil qualified

referenceTests :: Tasty.TestTree
referenceTests =
  Tasty.testGroup
    "reference edge cases"
    [ HUnit.testCase "relative root $id with a directory keeps # at the root" testRelativeRootIdCycle
    , HUnit.testCase "relative root $id with a directory resolves # under items" testRelativeRootIdItems
    , HUnit.testCase "relative refs resolve against a relative root $id" testRelativeRefAgainstRelativeId
    , HUnit.testCase "$id with an empty fragment is found by its absolute URI" testEmptyFragmentIdResolves
    , HUnit.testCase "registered $id with an empty fragment is found by its absolute URI" testEmptyFragmentIdRegistered
    , HUnit.testCase "escaped $defs name is not confused with a nested pointer" testEscapedDefsNameNoCycle
    , HUnit.testCase "additionalProperties next to $ref sees sibling properties" testAdditionalPropertiesBesideRef
    , HUnit.testCase "$ref with siblings applies without the applicator vocabulary" testRefWithoutApplicatorVocabulary
    , HUnit.testCase "URI $ref with a JSON-pointer fragment resolves" testUriPointerRef
    , HUnit.testCase "URI $dynamicRef with a JSON-pointer fragment resolves" testUriPointerDynamicRef
    , HUnit.testCase "URI $dynamicRef to an unknown anchor is unresolved" testUriDynamicRefUnknownAnchor
    , HUnit.testCase "local $dynamicRef with a JSON-pointer fragment resolves" testLocalPointerDynamicRef
    , HUnit.testCase "dereference keeps unknown keywords next to $ref" testDereferenceKeepsExtras
    , HUnit.testCase "a ref cycle under not is still reported" testCycleUnderNot
    , HUnit.testCase "a ref cycle under if is still reported" testCycleUnderIf
    , HUnit.testCase "a ref cycle in a failing anyOf branch is still reported" testCycleUnderAnyOf
    , HUnit.testCase "# inside a $dynamicRef target is the resource root" testDynamicTargetRootRef
    , HUnit.testCase "$defs refs inside a $dynamicRef target resolve in its resource" testDynamicTargetDefsRef
    , HUnit.testCase "a $dynamicRef that re-enters itself is a RefCycle" testDynamicRefSelfCycle
    , HUnit.testCase "a missing anchor does not bind to a metaschema anchor" testMissingAnchorIgnoresMetaschemas
    , HUnit.testCase "a missing $defs entry does not bind to a metaschema definition" testMissingDefIgnoresMetaschemas
    , HUnit.testCase "$dynamicRef applies next to a resolved $ref" testDynamicRefBesideRef
    , HUnit.testCase "dereference of a recursive schema is finite and equivalent" testDereferenceRecursive
    , HUnit.testCase "insertResource replaces a resource's anchors" testInsertResourceReplacesAnchors
    , HUnit.testCase "addSchema replaces a resource's anchors" testAddSchemaReplacesAnchors
    , HUnit.testCase "a root that shares a registered $id hides the registered anchors" testRootIdHidesRegisteredAnchors
    , HUnit.testCase "insertResource with an empty URI does not replace the root" testInsertEmptyUriKeepsRoot
    , HUnit.testCase "an id-less registered schema does not lend anchors to the root" testIdlessAddSchemaAnchors
    , HUnit.testCase "insertResource drops the fragment of its URI" testInsertResourceDropsFragment
    , HUnit.testCase "an array index past Int range does not wrap" testHugeArrayIndex
    , HUnit.testCase "checkSchema reports a $dynamicRef cycle" testCheckDynamicRefCycle
    , HUnit.testCase "checkSchema reports a missing $dynamicRef target" testCheckDynamicRefMissing
    , HUnit.testCase "checkSchema reports a $dynamicRef anchor that re-enters itself" testCheckDynamicAnchorSelfCycle
    , HUnit.testCase "checkSchema accepts a $dynamicRef to a known dynamic anchor" testCheckDynamicAnchorKnown
    , HUnit.testCase "checkSchema terminates on a deep diamond of shared refs" testCheckDiamondTerminates
    , HUnit.testCase "checkSchema checks targets reached through unknown keywords" testCheckUnknownKeywordTargets
    ]

testRelativeRootIdCycle :: HUnit.Assertion
testRelativeRootIdCycle =
  assertRejectsWith TestUtil.isRefCycle Validate.emptyRegistry "{\"$id\":\"schemas/tree.json\",\"$ref\":\"#\"}" "null"

assertRejectsWith ::
  (Validate.ValidationErrorKind -> Bool) -> Validate.Registry -> LBS.ByteString -> LBS.ByteString -> HUnit.Assertion
assertRejectsWith expected registry schemaBytes valueBytes = do
  errors <- validateWith registry schemaBytes valueBytes
  HUnit.assertBool ("unexpected errors: " <> show errors) (any (expected . Validate.errorKind) errors)

validateWith :: Validate.Registry -> LBS.ByteString -> LBS.ByteString -> IO [Validate.ValidationError]
validateWith registry schemaBytes valueBytes = do
  schema <- decodeSchema schemaBytes
  value <- decodeValue valueBytes
  TestUtil.withinTimeout (Validate.validate (Validate.compile registry schema) value)

decodeSchema :: LBS.ByteString -> IO Schema.Schema
decodeSchema bytes = case TestUtil.decodeSchema bytes of
  Left err -> HUnit.assertFailure ("schema decode failed: " <> err)
  Right schema -> pure schema

decodeValue :: LBS.ByteString -> IO Aeson.Value
decodeValue bytes = case Aeson.eitherDecode bytes of
  Left err -> HUnit.assertFailure ("value decode failed: " <> err)
  Right value -> pure value

testRelativeRootIdItems :: HUnit.Assertion
testRelativeRootIdItems = do
  schema <- decodeSchema "{\"$id\":\"schemas/tree.json\",\"type\":\"array\",\"items\":{\"$ref\":\"#\"}}"
  result <- TestUtil.withinTimeout (Validate.checkSchema Validate.emptyRegistry schema)
  result HUnit.@?= Right ()
  assertPartitions
    isTypeMismatch
    Validate.emptyRegistry
    "{\"$id\":\"schemas/tree.json\",\"type\":\"array\",\"items\":{\"$ref\":\"#\"}}"
    "[[],[[]]]"
    "[[1]]"

assertPartitions ::
  (Validate.ValidationErrorKind -> Bool) ->
  Validate.Registry ->
  LBS.ByteString ->
  LBS.ByteString ->
  LBS.ByteString ->
  HUnit.Assertion
assertPartitions expected registry schemaBytes accepted rejected = do
  assertAccepts registry schemaBytes accepted
  assertRejectsWith expected registry schemaBytes rejected

assertAccepts :: Validate.Registry -> LBS.ByteString -> LBS.ByteString -> HUnit.Assertion
assertAccepts registry schemaBytes valueBytes = do
  errors <- validateWith registry schemaBytes valueBytes
  HUnit.assertBool ("expected no errors, got: " <> show errors) (null errors)

isTypeMismatch :: Validate.ValidationErrorKind -> Bool
isTypeMismatch kind = case kind of
  Validate.TypeMismatch _ _ -> True
  _ -> False

testRelativeRefAgainstRelativeId :: HUnit.Assertion
testRelativeRefAgainstRelativeId = do
  registry <- registryWith ["{\"$id\":\"schemas/other.json\",\"type\":\"string\"}"]
  assertPartitions isTypeMismatch registry "{\"$id\":\"schemas/tree.json\",\"$ref\":\"other.json\"}" "\"x\"" "5"

registryWith :: [LBS.ByteString] -> IO Validate.Registry
registryWith documents = do
  schemas <- traverse decodeSchema documents
  pure (foldr Validate.addSchema Validate.emptyRegistry schemas)

testEmptyFragmentIdResolves :: HUnit.Assertion
testEmptyFragmentIdResolves = do
  let
    schema = "{\"$id\":\"http://x/s#\",\"type\":\"object\",\"properties\":{\"a\":{\"$ref\":\"http://x/s\"}}}"
  assertPartitions isTypeMismatch Validate.emptyRegistry schema "{\"a\":{}}" "{\"a\":1}"

testEmptyFragmentIdRegistered :: HUnit.Assertion
testEmptyFragmentIdRegistered = do
  registry <- registryWith ["{\"$id\":\"http://x/t#\",\"type\":\"string\"}"]
  assertPartitions isTypeMismatch registry "{\"$ref\":\"http://x/t\"}" "\"s\"" "1"

testEscapedDefsNameNoCycle :: HUnit.Assertion
testEscapedDefsNameNoCycle = do
  let
    schema = "{\"$defs\":{\"a/b\":{\"$ref\":\"#/$defs/a/b\"},\"a\":{\"b\":{\"type\":\"string\"}}},\"$ref\":\"#/$defs/a~1b\"}"
  assertAccepts Validate.emptyRegistry schema "\"ok\""
  errors <- validateWith Validate.emptyRegistry schema "1"
  HUnit.assertBool ("expected only a type error, got: " <> show errors)
    $ not (null errors) && not (any (TestUtil.isRefCycle . Validate.errorKind) errors)

testAdditionalPropertiesBesideRef :: HUnit.Assertion
testAdditionalPropertiesBesideRef = do
  let
    schema =
      "{\"$defs\":{\"a\":{\"type\":\"object\"}},\"$ref\":\"#/$defs/a\",\"properties\":{\"x\":true},\"additionalProperties\":false}"
  assertPartitions isFalseSchemaRejection Validate.emptyRegistry schema "{\"x\":1}" "{\"y\":1}"

isFalseSchemaRejection :: Validate.ValidationErrorKind -> Bool
isFalseSchemaRejection kind = case kind of
  Validate.FalseSchemaRejection -> True
  _ -> False

testRefWithoutApplicatorVocabulary :: HUnit.Assertion
testRefWithoutApplicatorVocabulary = do
  registry <-
    registryWith
      [ "{\"$id\":\"http://x/meta\",\"$vocabulary\":{\"https://json-schema.org/draft/2020-12/vocab/core\":true,\"https://json-schema.org/draft/2020-12/vocab/validation\":true}}"
      ]
  let
    schema = "{\"$schema\":\"http://x/meta\",\"$ref\":\"#/$defs/s\",\"$defs\":{\"s\":{\"type\":\"string\"}}}"
  assertPartitions isTypeMismatch registry schema "\"s\"" "5"

testUriPointerRef :: HUnit.Assertion
testUriPointerRef = do
  target <- decodeSchema "{\"properties\":{\"a\":{\"type\":\"string\"}}}"
  let
    registry = Validate.insertResource "http://x/other" target Validate.emptyRegistry
  assertPartitions isTypeMismatch registry "{\"$ref\":\"http://x/other#/properties/a\"}" "\"s\"" "1"

testUriPointerDynamicRef :: HUnit.Assertion
testUriPointerDynamicRef = do
  registry <- registryWith ["{\"$id\":\"http://x/item.json\",\"$defs\":{\"pos\":{\"minimum\":1}},\"type\":\"string\"}"]
  assertPartitions isMinimumFailed registry "{\"$dynamicRef\":\"http://x/item.json#/$defs/pos\"}" "5" "0"

isMinimumFailed :: Validate.ValidationErrorKind -> Bool
isMinimumFailed kind = case kind of
  Validate.MinimumFailed _ _ -> True
  _ -> False

testUriDynamicRefUnknownAnchor :: HUnit.Assertion
testUriDynamicRefUnknownAnchor = do
  registry <- registryWith ["{\"$id\":\"http://x/item.json\",\"type\":\"string\"}"]
  assertRejectsWith TestUtil.isRefUnresolved registry "{\"$dynamicRef\":\"http://x/item.json#missing\"}" "\"s\""

testLocalPointerDynamicRef :: HUnit.Assertion
testLocalPointerDynamicRef =
  assertPartitions
    isTypeMismatch
    Validate.emptyRegistry
    "{\"properties\":{\"x\":{\"type\":\"string\"},\"y\":{\"$dynamicRef\":\"#/properties/x\"}}}"
    "{\"y\":\"s\"}"
    "{\"y\":1}"

testDereferenceKeepsExtras :: HUnit.Assertion
testDereferenceKeepsExtras = do
  schema <-
    decodeSchema
      "{\"$defs\":{\"N\":{\"type\":\"string\"}},\"properties\":{\"a\":{\"$ref\":\"#/$defs/N\",\"x-ui\":\"hidden\",\"default\":\"q\"}}}"
  expected <-
    decodeValue
      "{\"$defs\":{\"N\":{\"type\":\"string\"}},\"properties\":{\"a\":{\"x-ui\":\"hidden\",\"default\":\"q\",\"allOf\":[{\"type\":\"string\"}]}}}"
  Aeson.toJSON (Validate.dereference Validate.emptyRegistry schema) HUnit.@?= expected

testCycleUnderNot :: HUnit.Assertion
testCycleUnderNot =
  assertRejectsWith TestUtil.isRefCycle Validate.emptyRegistry "{\"not\":{\"$ref\":\"#\"}}" "1"

testCycleUnderIf :: HUnit.Assertion
testCycleUnderIf =
  assertRejectsWith
    TestUtil.isRefCycle
    Validate.emptyRegistry
    "{\"if\":{\"$ref\":\"#\"},\"then\":false,\"else\":true}"
    "1"

testCycleUnderAnyOf :: HUnit.Assertion
testCycleUnderAnyOf =
  assertRejectsWith TestUtil.isRefCycle Validate.emptyRegistry "{\"anyOf\":[true,{\"$ref\":\"#\"}]}" "1"

testDynamicTargetRootRef :: HUnit.Assertion
testDynamicTargetRootRef =
  assertAccepts
    Validate.emptyRegistry
    "{\"$id\":\"http://x/root\",\"type\":[\"object\",\"number\"],\"$defs\":{\"a\":{\"$dynamicAnchor\":\"a\",\"anyOf\":[{\"type\":\"null\"},{\"$ref\":\"#\"}]}},\"properties\":{\"p\":{\"$dynamicRef\":\"#a\"}}}"
    "{\"p\":5}"

testDynamicTargetDefsRef :: HUnit.Assertion
testDynamicTargetDefsRef =
  assertPartitions
    isTypeMismatch
    Validate.emptyRegistry
    "{\"$id\":\"http://x/tree\",\"$defs\":{\"leaf\":{\"type\":\"string\"},\"node\":{\"$dynamicAnchor\":\"node\",\"type\":\"object\",\"properties\":{\"value\":{\"$ref\":\"#/$defs/leaf\"},\"child\":{\"$dynamicRef\":\"#node\"}}}},\"$ref\":\"#/$defs/node\"}"
    "{\"value\":\"x\",\"child\":{\"value\":\"y\"}}"
    "{\"value\":\"x\",\"child\":{\"value\":1}}"

testDynamicRefSelfCycle :: HUnit.Assertion
testDynamicRefSelfCycle =
  assertRejectsWith TestUtil.isRefCycle Validate.emptyRegistry "{\"$dynamicAnchor\":\"x\",\"$dynamicRef\":\"#x\"}" "1"

testMissingAnchorIgnoresMetaschemas :: HUnit.Assertion
testMissingAnchorIgnoresMetaschemas =
  assertRejectsWith TestUtil.isRefUnresolved Validate.emptyRegistry "{\"$ref\":\"#meta\"}" "1"

testMissingDefIgnoresMetaschemas :: HUnit.Assertion
testMissingDefIgnoresMetaschemas =
  assertRejectsWith TestUtil.isRefUnresolved Validate.emptyRegistry "{\"$dynamicRef\":\"#/$defs/stringArray\"}" "1"

testDynamicRefBesideRef :: HUnit.Assertion
testDynamicRefBesideRef =
  assertPartitions
    (const True)
    Validate.emptyRegistry
    "{\"$defs\":{\"s\":{\"type\":\"string\"},\"m\":{\"maxLength\":2}},\"$ref\":\"#/$defs/s\",\"$dynamicRef\":\"#/$defs/m\"}"
    "\"ab\""
    "\"abc\""

testDereferenceRecursive :: HUnit.Assertion
testDereferenceRecursive = do
  let
    treeBytes =
      "{\"$defs\":{\"node\":{\"type\":\"object\",\"properties\":{\"children\":{\"type\":\"array\",\"items\":{\"$ref\":\"#/$defs/node\"}}}}},\"$ref\":\"#/$defs/node\"}"
  schema <- decodeSchema treeBytes
  dereferenced <-
    TestUtil.withinTimeout (Aeson.toJSON (Validate.dereference Validate.emptyRegistry schema))
  dereferencedSchema <- decodeSchema (Aeson.encode dereferenced)
  good <- decodeValue "{\"children\":[{\"children\":[]},{\"children\":[{\"children\":[]}]}]}"
  bad <- decodeValue "{\"children\":[{\"children\":[1]}]}"
  validGood <- TestUtil.withinTimeout (Validate.isValidOn dereferencedSchema good)
  validBad <- TestUtil.withinTimeout (Validate.isValidOn dereferencedSchema bad)
  (validGood, validBad) HUnit.@?= (True, False)

testInsertResourceReplacesAnchors :: HUnit.Assertion
testInsertResourceReplacesAnchors = do
  first <- decodeSchema "{\"$anchor\":\"n\",\"type\":\"string\"}"
  second <- decodeSchema "{\"type\":\"number\"}"
  let
    registry =
      Validate.insertResource "http://x/a" second (Validate.insertResource "http://x/a" first Validate.emptyRegistry)
  assertRejectsWith TestUtil.isRefUnresolved registry "{\"$ref\":\"http://x/a#n\"}" "1"

testAddSchemaReplacesAnchors :: HUnit.Assertion
testAddSchemaReplacesAnchors = do
  registry <-
    registryWith
      [ "{\"$id\":\"http://x/a\",\"type\":\"number\"}"
      , "{\"$id\":\"http://x/a\",\"$anchor\":\"n\",\"type\":\"string\"}"
      ]
  assertRejectsWith TestUtil.isRefUnresolved registry "{\"$ref\":\"http://x/a#n\"}" "1"

testRootIdHidesRegisteredAnchors :: HUnit.Assertion
testRootIdHidesRegisteredAnchors = do
  registry <- registryWith ["{\"$id\":\"http://x/a\",\"$anchor\":\"n\",\"type\":\"string\"}"]
  assertRejectsWith TestUtil.isRefUnresolved registry "{\"$id\":\"http://x/a\",\"$ref\":\"#n\"}" "1"

testInsertEmptyUriKeepsRoot :: HUnit.Assertion
testInsertEmptyUriKeepsRoot = do
  other <- decodeSchema "{\"type\":\"string\"}"
  assertAccepts (Validate.insertResource "" other Validate.emptyRegistry) "{\"type\":\"integer\"}" "5"

testIdlessAddSchemaAnchors :: HUnit.Assertion
testIdlessAddSchemaAnchors = do
  registry <- registryWith ["{\"properties\":{\"x\":{\"$anchor\":\"foo\",\"type\":\"string\"}}}"]
  schema <- decodeSchema "{\"$ref\":\"#foo\"}"
  assertRejectsWith TestUtil.isRefUnresolved registry "{\"$ref\":\"#foo\"}" "5"
  result <- TestUtil.withinTimeout (Validate.checkSchema registry schema)
  result HUnit.@?= Left (Validate.UnresolvableReferenceError "#foo")

testInsertResourceDropsFragment :: HUnit.Assertion
testInsertResourceDropsFragment = do
  target <- decodeSchema "{\"type\":\"string\"}"
  assertPartitions
    isTypeMismatch
    (Validate.insertResource "http://x/s#" target Validate.emptyRegistry)
    "{\"$ref\":\"http://x/s\"}"
    "\"a\""
    "1"

testHugeArrayIndex :: HUnit.Assertion
testHugeArrayIndex =
  assertRejectsWith
    TestUtil.isRefUnresolved
    Validate.emptyRegistry
    "{\"prefixItems\":[{\"type\":\"string\"}],\"items\":{\"$ref\":\"#/prefixItems/18446744073709551616\"}}"
    "[\"a\",1]"

testCheckDynamicRefCycle :: HUnit.Assertion
testCheckDynamicRefCycle =
  assertCheck
    "{\"$defs\":{\"a\":{\"$dynamicRef\":\"#/$defs/a\"}},\"$dynamicRef\":\"#/$defs/a\"}"
    (Left (Validate.ReferenceCycleError "#/$defs/a"))

assertCheck :: LBS.ByteString -> Either Validate.SchemaError () -> HUnit.Assertion
assertCheck schemaBytes expected = do
  schema <- decodeSchema schemaBytes
  result <- TestUtil.withinTimeout (Validate.checkSchema Validate.emptyRegistry schema)
  result HUnit.@?= expected

testCheckDynamicRefMissing :: HUnit.Assertion
testCheckDynamicRefMissing =
  assertCheck "{\"$dynamicRef\":\"#/$defs/missing\"}" (Left (Validate.UnresolvableReferenceError "#/$defs/missing"))

testCheckDynamicAnchorSelfCycle :: HUnit.Assertion
testCheckDynamicAnchorSelfCycle =
  assertCheck "{\"$dynamicAnchor\":\"x\",\"$dynamicRef\":\"#x\"}" (Left (Validate.ReferenceCycleError "#x"))

testCheckDynamicAnchorKnown :: HUnit.Assertion
testCheckDynamicAnchorKnown =
  assertCheck
    "{\"$defs\":{\"n\":{\"$dynamicAnchor\":\"node\",\"type\":\"object\"}},\"items\":{\"$dynamicRef\":\"#node\"}}"
    (Right ())

testCheckDiamondTerminates :: HUnit.Assertion
testCheckDiamondTerminates =
  let
    depth = 40 :: Int
    name i = Key.fromString ("a" <> show i)
    refTo i = Aeson.object ["$ref" Aeson..= ("#/$defs/a" <> show i)]
    definition i
      | i == depth = name i Aeson..= True
      | otherwise = name i Aeson..= Aeson.object ["allOf" Aeson..= [refTo (i + 1), refTo (i + 1)]]
    schema = Aeson.object ["$defs" Aeson..= Aeson.object (fmap definition [0 .. depth]), "$ref" Aeson..= ("#/$defs/a0" :: String)]
   in
    assertCheck (Aeson.encode schema) (Right ())

testCheckUnknownKeywordTargets :: HUnit.Assertion
testCheckUnknownKeywordTargets =
  assertCheck
    "{\"$ref\":\"#/definitions/A\",\"definitions\":{\"A\":{\"properties\":{\"b\":{\"$ref\":\"#/definitions/Missing\"}}}}}"
    (Left (Validate.UnresolvableReferenceError "#/definitions/Missing"))
