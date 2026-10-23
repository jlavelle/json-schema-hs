module Test (tree) where

import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString.Lazy qualified as LBS
import Data.Char qualified as Char
import Data.Either qualified as Either
import Data.Foldable qualified as Foldable
import Data.HashSet qualified as HashSet
import Data.List.NonEmpty qualified as NEL
import Data.Map.Ordered qualified as OMap
import Data.Map.Strict qualified as Map
import Data.Maybe qualified as Maybe
import Data.Scientific qualified as Scientific
import Data.Set qualified as Set
import Data.Set.Ordered qualified as OSet
import Data.Text qualified as Text
import Data.Vector qualified as Vector
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import JsonSchema qualified as Schema
import JsonSchema.Validate qualified as Validate
import JsonSchema.Validate.Array qualified as Array
import JsonSchema.Validate.Dialect qualified as Dialect
import JsonSchema.Validate.Format qualified as Format
import JsonSchema.Validate.Metaschema qualified as Metaschema
import JsonSchema.Validate.Registry qualified as Registry
import JsonSchema.Validate.Resolve.Prepass qualified as Prepass
import JsonSchema.Validate.Uri qualified as Uri
import System.Directory qualified as Directory
import System.FilePath qualified as FilePath
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit qualified as HUnit
import Test.Tasty.Hedgehog qualified as TastyH

import ReferenceTests qualified
import SchemaGen qualified
import TestUtil qualified

tree :: IO Tasty.TestTree
tree = do
  remoteRegistry <- loadRemoteSchemas
  driftTest <- suiteDriftTest
  suiteTests <- loadTestSuite remoteRegistry
  optionalTests <- loadOptionalTestSuite remoteRegistry
  pure
    $ Tasty.testGroup
      "json-schema"
      [ Tasty.testGroup
          "properties"
          [ Tasty.testGroup
              "boolean schemas"
              [ TastyH.testProperty "true schema accepts all values" propTrueAcceptsAll
              , TastyH.testProperty "false schema rejects all values" propFalseRejectsAll
              , TastyH.testProperty "empty schema accepts all values" propEmptyAcceptsAll
              ]
          , Tasty.testGroup
              "type validation"
              [ HUnit.testCase "null validates as null" testNullType
              , TastyH.testProperty "bool validates as boolean" propBoolType
              , TastyH.testProperty "string validates as string" propStringType
              , TastyH.testProperty "number validates as number" propNumberType
              , TastyH.testProperty "integer validates as integer" propIntegerType
              , HUnit.testCase "integer accepts 1.0 (Scientific.isInteger)" testIntegerAcceptsOnePointZero
              , HUnit.testCase "integer rejects 1.5" testIntegerRejectsOnePointFive
              , TastyH.testProperty "array validates as array" propArrayType
              , TastyH.testProperty "object validates as object" propObjectType
              , HUnit.testCase "string rejects number with TypeMismatch" testStringRejectsNumber
              , HUnit.testCase "number rejects string with TypeMismatch" testNumberRejectsString
              , HUnit.testCase "boolean rejects null with TypeMismatch" testBooleanRejectsNull
              , HUnit.testCase "null rejects bool with TypeMismatch" testNullRejectsBool
              , HUnit.testCase "array rejects object with TypeMismatch" testArrayRejectsObject
              , HUnit.testCase "object rejects array with TypeMismatch" testObjectRejectsArray
              ]
          , Tasty.testGroup
              "numeric constraints"
              [ TastyH.testProperty "minimum" propMinimum
              , TastyH.testProperty "maximum" propMaximum
              , TastyH.testProperty "exclusive minimum" propExclusiveMinimum
              , TastyH.testProperty "exclusive maximum" propExclusiveMaximum
              ]
          , Tasty.testGroup
              "string constraints"
              [ TastyH.testProperty "minLength" propMinLength
              , TastyH.testProperty "maxLength" propMaxLength
              , TastyH.testProperty "pattern (unicode-aware)" propPatternUnicode
              ]
          , Tasty.testGroup
              "array constraints"
              [ TastyH.testProperty "minItems" propMinItems
              , TastyH.testProperty "maxItems" propMaxItems
              , TastyH.testProperty "uniqueItems law" propUniqueItems
              ]
          , Tasty.testGroup
              "contains bounds"
              [ TastyH.testProperty "minContains rejects below bound" propMinContainsRejectsBelow
              , TastyH.testProperty "maxContains rejects above bound" propMaxContainsRejectsAbove
              , TastyH.testProperty "minContains: 0 permits arrays with no matches" propMinZeroPermitsEmpty
              , HUnit.testCase "bare contains rejects zero matches via ContainsFailed" testBareContainsRejectsZero
              , HUnit.testCase "bare contains accepts one match" testBareContainsAcceptsOne
              ]
          , Tasty.testGroup
              "unevaluatedItems"
              [ TastyH.testProperty "sees prefixItems annotations" propUnevaluatedItemsSeesPrefixItems
              , TastyH.testProperty "sees items annotations" propUnevaluatedItemsSeesItems
              , TastyH.testProperty "sees contains annotations" propUnevaluatedItemsSeesContains
              , TastyH.testProperty "ignores non-arrays" propUnevaluatedItemsIgnoresNonArrays
              , TastyH.testProperty "sees annotations across allOf" propUnevaluatedItemsAcrossAllOf
              , TastyH.testProperty "annotations stay scoped to their instance location" propUnevaluatedItemsInstanceLocation
              , TastyH.testProperty "field survives codec round-trip" propUnevaluatedItemsCodecRoundTrip
              ]
          , Tasty.testGroup
              "object constraints"
              [ TastyH.testProperty "required present accepts" propRequiredPresent
              , HUnit.testCase "required missing rejects" testRequiredMissing
              , TastyH.testProperty "additionalProperties false law" propAdditionalPropertiesFalse
              ]
          , Tasty.testGroup
              "composition"
              [ TastyH.testProperty "allOf law" propAllOf
              , TastyH.testProperty "anyOf law" propAnyOf
              , TastyH.testProperty "not inverts validation" propNot
              ]
          , Tasty.testGroup
              "conditional"
              [ TastyH.testProperty "if/then/else law" propIfThenElse
              ]
          , Tasty.testGroup
              "enum and const"
              [ TastyH.testProperty "const matches exact value" propConst
              , TastyH.testProperty "enum matches one of values" propEnum
              ]
          , Tasty.testGroup
              "roundtrip"
              [ TastyH.testProperty "parse/encode roundtrip" propParseRoundtrip
              ]
          , Tasty.testGroup
              "totality"
              [ TastyH.testProperty "validate is total over arbitrary schema and value" propValidateTotal
              , TastyH.testProperty "unresolvable $ref produces RefUnresolved fault" propUnresolvableRef
              ]
          , Tasty.testGroup
              "pinned behaviour"
              [ TastyH.testProperty
                  "dependencies (legacy dependentRequired) rejects when dependent missing"
                  propDependentRequiredRejects
              , TastyH.testProperty
                  "dependentRequired rejects when dependent missing"
                  propDependentRequiredStandaloneRejects
              , TastyH.testProperty
                  "dependentRequired accepts when trigger absent"
                  propDependentRequiredTriggerAbsent
              , TastyH.testProperty
                  "dependentRequired ignores non-objects"
                  propDependentRequiredIgnoresNonObjects
              , TastyH.testProperty
                  "dependentRequired keys survive codec round-trip"
                  propDependentRequiredEscapedKeys
              , TastyH.testProperty
                  "dependentRequired emits one fault per missing dependent"
                  propDependentRequiredOneFaultPerMissing
              ]
          , Tasty.testGroup
              "resolver and prepass pins"
              [ TastyH.testProperty
                  "resolveUri with no base returns the reference unchanged"
                  propResolveUriEmptyPre
              , TastyH.testProperty
                  "Array.hasUniqueItems matches HashSet-based distinctness"
                  propHasUniqueItemsLaw
              , HUnit.testCase
                  "multi-document dynamicRef resolves outermost dynamicAnchor"
                  testMultiDocDynamicRef
              ]
          , Tasty.testGroup
              "searchDefsGlobally"
              [ HUnit.testCase
                  "resolution tracks dynamic scope, not $id-alphabetical order (forward)"
                  (testSearchDefsScopeAnchored False)
              , HUnit.testCase
                  "resolution tracks dynamic scope, not $id-alphabetical order (reversed)"
                  (testSearchDefsScopeAnchored True)
              ]
          , Tasty.testGroup
              "compile-once"
              [ HUnit.testCase
                  "invalid pattern surfaces as validation-time PatternCompileFailed"
                  testPatternCompileFailedSurvives
              ]
          , Tasty.testGroup
              "pattern"
              [ HUnit.testCase "anchors match the string ends, not line ends" testPatternAnchorsWholeString
              ]
          , Tasty.testGroup
              "$dynamicRef beside $ref"
              [ HUnit.testCase "local JSON pointer resolves" testDynamicPointerBesideRef
              , HUnit.testCase "URI-qualified JSON pointer resolves" testDynamicUriPointerBesideRef
              ]
          , Tasty.testGroup
              "schema paths name the failing location"
              [ HUnit.testCase "false schema has no keyword" testFalseSchemaPath
              , HUnit.testCase "uncompilable patternProperties key" testPatternPropertiesCompilePath
              , HUnit.testCase "unresolved $dynamicRef" testDynamicRefUnresolvedKeyword
              , HUnit.testCase "patternProperties subschema" testPatternPropertiesPath
              , HUnit.testCase "dependentSchemas subschema" testDependentSchemasPath
              , HUnit.testCase "$ref cycle sits at the $ref" testRefCyclePath
              , HUnit.testCase "errors inside a $dynamicRef target" testDynamicRefTargetPath
              ]
          , Tasty.testGroup
              "contains and propertyNames instance paths"
              [ HUnit.testCase "contains descends into each item" testContainsDescends
              , HUnit.testCase "propertyNames descends into each name" testPropertyNamesDescends
              ]
          , Tasty.testGroup
              "validateResolvedSchema locator"
              [ TastyH.testProperty
                  "errors raised inside a $ref target carry the caller's instance path"
                  propResolvedRefLocator
              ]
          , Tasty.testGroup
              "root self-ref siblings"
              [ HUnit.testCase
                  "{\"$ref\":\"#\",\"type\":\"object\"} rejects array"
                  testRootSelfRefTypeObjectRejectsArray
              , HUnit.testCase
                  "{\"$ref\":\"#\",\"type\":\"string\"} rejects number"
                  testRootSelfRefTypeStringRejectsNumber
              , TastyH.testProperty
                  "root $ref:\"#\" reports a cycle and enforces sibling type assertion"
                  propRootSelfRefSiblingTypeEnforced
              ]
          , Tasty.testGroup
              "defs-ref siblings (non-subset keywords)"
              [ HUnit.testCase
                  "{$ref:#/$defs/x,minLength:5} enforces minLength (identity branch)"
                  testDefsRefSiblingMinLengthEnforced
              , HUnit.testCase
                  "{$ref:#/$defs/n,minimum:10} enforces minimum (identity branch)"
                  testDefsRefSiblingMinimumEnforced
              , HUnit.testCase
                  "property {$ref:#/$defs/x,minLength:5} enforces minLength (otherwise branch)"
                  testDefsRefSiblingViaPropertyEnforced
              ]
          , Tasty.testGroup
              "non-http absolute $ref resolution"
              [ HUnit.testCase
                  "resolveUri passes absolute non-http schemes through"
                  testResolveUriPassesAbsoluteSchemes
              , HUnit.testCase
                  "urn $ref resolves under an http base"
                  testNonHttpAbsoluteRefResolves
              ]
          , Tasty.testGroup
              "reference resolution"
              [ HUnit.testCase
                  "resolveUri: RFC 3986 section 5.4.1 table"
                  testResolveUriRfc3986
              , TastyH.testProperty
                  "percentDecode inverts percentEncode"
                  propPercentDecodeRoundtrip
              , HUnit.testCase
                  "percentDecode pins (quote, percent, tilde interplay, malformed)"
                  testPercentDecodePins
              , HUnit.testCase
                  "parseSimpleRef decodes percent and tilde escapes"
                  testParseSimpleRefDecoding
              , HUnit.testCase
                  "bare #/$defs/Name resolves in the enclosing resource"
                  testPerResourceDefs
              , HUnit.testCase
                  "URI ref with anchor fragment resolves the anchor, not the document"
                  testUriRefAnchorFragment
              , HUnit.testCase
                  "same anchor name under different bases resolves by base"
                  testSameAnchorDifferentBase
              , HUnit.testCase
                  "refs inside a foreign target resolve in the target's context"
                  testForeignContextDefs
              , HUnit.testCase
                  "foreign $ref \"#\" rewrites to the resource's canonical URI (white-box)"
                  testForeignHashRewriteWhiteBox
              , HUnit.testCase
                  "foreign $ref \"#\" means the foreign root (black-box)"
                  testForeignHashRewriteBlackBox
              , HUnit.testCase
                  "late-bound target ties its own root knot"
                  testLateBoundTargetKnot
              , HUnit.testCase
                  "cached target ties its own root knot"
                  testCachedTargetKnot
              ]
          ]
      , Tasty.testGroup
          "reference completeness"
          [ HUnit.testCase "embedded metaschemas decode" testEmbeddedMetaschemasDecode
          , HUnit.testCase "2020-12 metaschema yields the standard vocabularies" testMetaschemaVocabularies
          , HUnit.testCase "array index pointer segments reject leading zeros" testReadMaybeIntLeadingZeros
          , HUnit.testCase "$defs self-ref is a RefCycle" testDefsSelfCycle
          , HUnit.testCase "$defs mutual ref is a RefCycle" testDefsMutualCycle
          , HUnit.testCase "allOf self-ref to root is a RefCycle" testAllOfRootCycle
          , HUnit.testCase "URI self-ref is a RefCycle" testUriSelfCycle
          , HUnit.testCase "anchor self-ref is a RefCycle" testAnchorSelfCycle
          , HUnit.testCase "items-recursive tree validates a nested value" testRecursiveTreeValidates
          ]
      , Tasty.testGroup
          "checkSchema"
          [ HUnit.testCase "uncompilable pattern is an InvalidPatternError" testCheckInvalidPattern
          , HUnit.testCase "missing $defs entry is an UnresolvableReferenceError" testCheckMissingDef
          , HUnit.testCase "missing anchor is an UnresolvableReferenceError" testCheckMissingAnchor
          , HUnit.testCase "registered anchor resolves" testCheckRegisteredAnchor
          , HUnit.testCase "plain $dynamicRef to a registered $dynamicAnchor passes" testCheckPlainDynamicRef
          , HUnit.testCase
              "$dynamicRef into an unknown resource is an UnresolvableReferenceError"
              testCheckDynamicRefUnknownResource
          , HUnit.testCase "root # under a moving keyword passes" testCheckRootRef
          , HUnit.testCase "$defs mutual ref is a ReferenceCycleError" testCheckCycle
          , HUnit.testCase "items-recursive tree passes" testCheckRecursiveTree
          ]
      , Tasty.testGroup
          "decoder lexical checks"
          [ HUnit.testCase "anchor starting with a digit is rejected" testDecodeRejectsBadAnchor
          , HUnit.testCase "dynamic anchor with a slash is rejected" testDecodeRejectsBadDynamicAnchor
          , HUnit.testCase "$id with a non-empty fragment is rejected" testDecodeRejectsIdFragment
          , HUnit.testCase "$id with an empty fragment is accepted" testDecodeAcceptsEmptyIdFragment
          ]
      , Tasty.testGroup
          "dereference"
          [HUnit.testCase "inlines $defs refs and leaves unresolvable refs" testDereference]
      , Tasty.testGroup
          "custom formats"
          [ HUnit.testCase "custom format validates" testCustomFormat
          , HUnit.testCase "registered format asserts under the default dialect" testCustomFormatRejects
          , HUnit.testCase "registered override of a builtin format asserts" testOverrideBuiltinFormat
          ]
      , Tasty.testGroup
          "builtin formats"
          [ TastyH.testProperty "uri accepts any Text after a scheme" propUriAcceptsScheme
          , HUnit.testCase "uri rejects relative references" testUriRejectsRelative
          , TastyH.testProperty "uri-reference accepts any non-empty Text" propUriReferenceRelaxed
          , HUnit.testCase "uri-reference rejects empty" testUriReferenceEmpty
          , HUnit.testCase "iri is not a builtin format" testIriNotBuiltin
          , HUnit.testCase "iri-reference is not a builtin format" testIriReferenceNotBuiltin
          , HUnit.testCase "hostname accepts ascii" testHostnameAscii
          , HUnit.testCase "hostname accepts a punycode label" testHostnamePunycode
          , HUnit.testCase "hostname rejects leading hyphen" testHostnameLeadingHyphen
          , HUnit.testCase "hostname rejects raw unicode label" testHostnameUnicodeRejected
          , HUnit.testCase "idn-hostname collapses to ascii hostname" testIdnHostnameAscii
          , HUnit.testCase "idn-hostname rejects raw unicode" testIdnHostnameUnicodeRejected
          , HUnit.testCase "idn-email collapses to ascii email" testIdnEmailAscii
          , HUnit.testCase "idn-email rejects raw unicode local part" testIdnEmailUnicodeRejected
          , HUnit.testCase "date accepts ISO date" testDateOk
          , HUnit.testCase "date rejects junk" testDateBad
          , HUnit.testCase "date requires a four-digit year" testDateYearDigits
          , HUnit.testCase "time accepts ISO time" testTimeOk
          , HUnit.testCase "time rejects junk" testTimeBad
          , HUnit.testCase "time checks ranges on leap seconds" testTimeLeapSecondRanges
          , HUnit.testCase "date-time accepts ISO datetime" testDateTimeOk
          , HUnit.testCase "date-time rejects junk" testDateTimeBad
          , HUnit.testCase "duration accepts ISO 8601" testDurationOk
          , HUnit.testCase "duration rejects empty P" testDurationBad
          , HUnit.testCase "email accepts simple" testEmailOk
          , HUnit.testCase "email rejects missing at" testEmailBad
          , HUnit.testCase "ipv4 accepts dotted quad" testIpv4Ok
          , HUnit.testCase "ipv4 rejects out of range" testIpv4Bad
          , HUnit.testCase "ipv6 accepts compressed" testIpv6Ok
          , HUnit.testCase "ipv6 rejects malformed" testIpv6Bad
          , HUnit.testCase "uri-template accepts braces" testUriTemplateOk
          , HUnit.testCase "uri-template rejects unbalanced" testUriTemplateBad
          , HUnit.testCase "json-pointer accepts empty and rooted" testJsonPointerOk
          , HUnit.testCase "json-pointer rejects unrooted" testJsonPointerBad
          , HUnit.testCase "json-pointer rejects a tilde without an escape digit" testJsonPointerBadEscape
          , HUnit.testCase "relative-json-pointer accepts numeric prefix" testRelJsonPointerOk
          , HUnit.testCase "relative-json-pointer rejects no prefix" testRelJsonPointerBad
          , HUnit.testCase "regex accepts simple pattern" testRegexOk
          , HUnit.testCase "regex rejects unbalanced group" testRegexBad
          , HUnit.testCase "uuid accepts v4" testUuidOk
          , HUnit.testCase "uuid rejects wrong length" testUuidBad
          ]
      , Tasty.testGroup
          "format vocabulary gating"
          [ HUnit.testCase "no $schema: format annotates only" testFormatAnnotationDefault
          , HUnit.testCase "unregistered canonical metaschema: annotation" testFormatCanonicalMetaschemaDefault
          , HUnit.testCase "format-assertion declared false still asserts" testFormatVocabularyFalseAsserts
          , HUnit.testCase "format-assertion declared true asserts" testFormatVocabularyTrueAsserts
          , HUnit.testCase "assertion recurses through $vocabulary-less metaschemas" testFormatVocabularyRecursion
          , HUnit.testCase "self-referential metaschema terminates in annotation" testFormatSelfReferentialMetaschema
          , HUnit.testCase "root $schema governs nested subschemas" testFormatDialectInheritance
          , HUnit.testCase "nested resource $schema replaces the dialect" testFormatDialectOverride
          , HUnit.testCase "a $dynamicRef target keeps its resource's dialect" testFormatDynamicTargetDialect
          , HUnit.testCase "withFormatAssertion opts into assertion" testFormatAssertionFlag
          , HUnit.testCase "non-string instance ignored under assertion" testFormatNonStringIgnored
          , TastyH.testProperty "annotation mode never asserts" propFormatAnnotationNeverAsserts
          ]
      , Tasty.testGroup
          "vocabulary gating"
          [ HUnit.testCase "applicator+core metaschema: minimum ignored" testVocabularyMinimumIgnored
          , HUnit.testCase "applicator+core metaschema: properties still rejects" testVocabularyPropertiesLive
          , HUnit.testCase "default dialect (no $schema): minimum enforced" testDefaultDialectEnforcesMinimum
          , HUnit.testCase "$schema with an empty fragment uses the registered metaschema" testVocabularyEmptyFragment
          ]
      , Tasty.testGroup
          "error details"
          [ HUnit.testCase "type error has keyword and schema path" testTypeErrorDetails
          , HUnit.testCase "required error has keyword and schema path" testRequiredErrorDetails
          , HUnit.testCase "nested property error has correct paths" testNestedPropertyErrorDetails
          , HUnit.testCase "minimum error has keyword" testMinimumErrorDetails
          , HUnit.testCase "format error has keyword" testFormatErrorDetails
          , HUnit.testCase "anyOf error has keyword" testAnyOfErrorDetails
          , HUnit.testCase "oneOf error has keyword" testOneOfErrorDetails
          , HUnit.testCase "not error has keyword" testNotErrorDetails
          , HUnit.testCase "if/then error propagates" testIfThenErrorDetails
          , HUnit.testCase "dependentSchemas error propagates" testDependentSchemasErrorDetails
          , HUnit.testCase "dependentRequired error has keyword and schema path" testDependentRequiredErrorDetails
          , HUnit.testCase "unevaluatedItems error has keyword and schema path" testUnevaluatedItemsErrorDetails
          , HUnit.testCase "deeply nested schema path" testDeeplyNestedSchemaPath
          , HUnit.testCase "array item instance path" testArrayItemInstancePath
          , HUnit.testCase "nested object instance path" testNestedObjectInstancePath
          , HUnit.testCase "deeply nested instance path" testDeeplyNestedInstancePath
          , HUnit.testCase "additionalProperties schema path" testAdditionalPropsSchemaPath
          , HUnit.testCase "allOf schema path propagation" testAllOfSchemaPath
          , HUnit.testCase "prefixItems instance path" testPrefixItemsInstancePath
          , HUnit.testCase "if/else paths" testIfElsePaths
          , HUnit.testCase "enum fault quotes offending value" testEnumFaultRenderQuotesValue
          , HUnit.testCase "const fault quotes both values" testConstFaultRenderQuotesValues
          ]
      , Tasty.testGroup
          "error context and rendering"
          [ HUnit.testCase "anyOf context holds every branch's errors" testAnyOfContext
          , HUnit.testCase "oneOf with zero matches has every branch's errors as context" testOneOfZeroContext
          , HUnit.testCase "oneOf with two matches has empty context" testOneOfManyContext
          , HUnit.testCase "renderJsonPointer escapes ~ and /" testRenderJsonPointerEscapes
          , TastyH.testProperty "renderJsonPointer round-trips through unescape" propRenderJsonPointerRoundTrip
          , HUnit.testCase "renderErrors separates errors with newlines" testRenderErrorsNewlines
          , HUnit.testCase "renderValidationError shows (root) for an empty instance path" testRenderRootPath
          ]
      , Tasty.testGroup "JSON-Schema-Test-Suite" suiteTests
      , Tasty.testGroup "optional" optionalTests
      , driftTest
      , ReferenceTests.referenceTests
      ]

loadRemoteSchemas :: IO Validate.Registry
loadRemoteSchemas = do
  files <- TestUtil.findJsonFiles remotesDir
  Foldable.foldlM loadOneRemote Validate.emptyRegistry files

remotesDir :: FilePath
remotesDir = TestUtil.testSuitePath "remotes"

loadOneRemote :: Validate.Registry -> FilePath -> IO Validate.Registry
loadOneRemote registry path = do
  bytes <- LBS.readFile path
  case TestUtil.decodeSchema bytes of
    Left _ -> pure registry
    Right schema ->
      let
        relPath = Text.pack . FilePath.makeRelative remotesDir $ path
        url = remoteBaseUrl <> relPath
       in
        pure (Validate.insertResource url schema registry)

remoteBaseUrl :: Text.Text
remoteBaseUrl = Text.pack "http://localhost:1234/"

suiteDriftTest :: IO Tasty.TestTree
suiteDriftTest = do
  topLevel <- listJsonFilesShallow suiteDir
  optionalTop <- listJsonFilesShallow (suiteDir FilePath.</> "optional")
  optionalFormat <- listJsonFilesShallow (suiteDir FilePath.</> "optional" FilePath.</> "format")
  let
    onDisk =
      Set.fromList
        $ topLevel
          <> fmap ("optional/" <>) optionalTop
          <> fmap ("optional/format/" <>) optionalFormat
    accountedFor =
      Set.fromList supportedSuiteFiles
        <> Set.fromList optionalSuiteFiles
        <> Set.fromList optionalFormatSuiteFiles
        <> knownExcludedSuiteFiles
    missing = Set.difference onDisk accountedFor
  pure . HUnit.testCase "suite file drift"
    $ HUnit.assertEqual
      ( "JSON-Schema-Test-Suite drift detected. Files present on disk but not"
          <> " mentioned in supportedSuiteFiles, optionalSuiteFiles,"
          <> " optionalFormatSuiteFiles, or knownExcludedSuiteFiles:"
          <> foldMap ("\n  " <>) (Set.toAscList missing)
          <> "\nDecide for each: add to supportedSuiteFiles (passes cleanly),"
          <> " or optionalSuiteFiles (passes under optional), or"
          <> " optionalFormatSuiteFiles (format-assertion enabled), or"
          <> " knownExcludedSuiteFiles (with a comment explaining why)."
      )
      Set.empty
      missing

listJsonFilesShallow :: FilePath -> IO [String]
listJsonFilesShallow dir = do
  exists <- Directory.doesDirectoryExist dir
  if not exists
    then pure []
    else do
      entries <- Directory.listDirectory dir
      pure
        . Maybe.mapMaybe
          ( \e ->
              if FilePath.takeExtension e == ".json"
                then Just (FilePath.dropExtension e)
                else Nothing
          )
        $ entries

suiteDir :: FilePath
suiteDir = TestUtil.testSuitePath "tests/draft2020-12"

supportedSuiteFiles :: [String]
supportedSuiteFiles =
  [ "additionalProperties"
  , "allOf"
  , "anchor"
  , "anyOf"
  , "boolean_schema"
  , "const"
  , "contains"
  , "content"
  , "default"
  , "defs"
  , "dependentRequired"
  , "dependentSchemas"
  , "dynamicRef"
  , "enum"
  , "exclusiveMaximum"
  , "exclusiveMinimum"
  , "format"
  , "if-then-else"
  , "infinite-loop-detection"
  , "items"
  , "maxContains"
  , "maximum"
  , "maxItems"
  , "maxLength"
  , "maxProperties"
  , "minContains"
  , "minimum"
  , "minItems"
  , "minLength"
  , "minProperties"
  , "multipleOf"
  , "not"
  , "oneOf"
  , "pattern"
  , "patternProperties"
  , "prefixItems"
  , "properties"
  , "propertyNames"
  , "ref"
  , "refRemote"
  , "required"
  , "type"
  , "unevaluatedItems"
  , "unevaluatedProperties"
  , "uniqueItems"
  , "vocabulary"
  ]

optionalSuiteFiles :: [String]
optionalSuiteFiles =
  [ "optional/anchor"
  , "optional/bignum"
  , "optional/dependencies-compatibility"
  , "optional/dynamicRef"
  , "optional/float-overflow"
  , "optional/format-assertion"
  , "optional/id"
  , "optional/no-schema"
  , "optional/non-bmp-regex"
  , "optional/refOfUnknownKeyword"
  , "optional/unknownKeyword"
  ]

optionalFormatSuiteFiles :: [String]
optionalFormatSuiteFiles =
  [ "optional/format/date"
  , "optional/format/date-time"
  , "optional/format/duration"
  , "optional/format/email"
  , "optional/format/ipv4"
  , "optional/format/ipv6"
  , "optional/format/json-pointer"
  , "optional/format/regex"
  , "optional/format/relative-json-pointer"
  , "optional/format/time"
  , "optional/format/unknown"
  , "optional/format/uri-template"
  , "optional/format/uuid"
  ]

knownExcludedSuiteFiles :: Set.Set String
knownExcludedSuiteFiles =
  Set.fromList
    [ "optional/format/hostname"
    , "optional/format/iri"
    , "optional/format/iri-reference"
    , "optional/format/uri"
    , "optional/format/uri-reference"
    , "optional/format/ecmascript-regex"
    , "optional/format/idn-email"
    , "optional/format/idn-hostname"
    , "optional/ecmascript-regex"
    , "optional/cross-draft"
    ]

loadTestSuite :: Validate.Registry -> IO [Tasty.TestTree]
loadTestSuite registry =
  traverse (loadSuiteFile registry) supportedSuiteFiles

loadSuiteFile :: Validate.Registry -> String -> IO Tasty.TestTree
loadSuiteFile registry name = do
  let
    path = suiteDir FilePath.</> name FilePath.<.> "json"
  bytes <- LBS.readFile path
  case Aeson.eitherDecode bytes of
    Left err -> pure . Tasty.testGroup name $ [HUnit.testCase "parse" . HUnit.assertFailure $ err]
    Right groups -> pure . Tasty.testGroup name $ fmap (mkTestGroup name registry) groups

mkTestGroup :: String -> Validate.Registry -> TestGroup -> Tasty.TestTree
mkTestGroup suiteName registry group =
  Tasty.testGroup (Text.unpack . testGroupDescription $ group)
    $ case Map.lookup (suiteName, testGroupDescription group) skippedSuiteGroups of
      Just reason -> [HUnit.testCase ("skipped: " <> Text.unpack reason) $ pure ()]
      Nothing -> case TestUtil.decodeSchemaValue . testGroupSchema $ group of
        Left err ->
          [HUnit.testCase "schema decode" . HUnit.assertFailure $ err]
        Right schema ->
          fmap (mkTestCase registry schema) . testGroupTests $ group

data TestGroup = TestGroup
  { testGroupDescription :: Text.Text
  , testGroupSchema :: Aeson.Value
  , testGroupTests :: [TestCase]
  }

instance Aeson.FromJSON TestGroup where
  parseJSON = Aeson.withObject "TestGroup" $ \obj -> do
    description <- obj Aeson..: Key.fromText (Text.pack "description")
    schema <- obj Aeson..: Key.fromText (Text.pack "schema")
    tests <- obj Aeson..: Key.fromText (Text.pack "tests")
    pure TestGroup{testGroupDescription = description, testGroupSchema = schema, testGroupTests = tests}

data TestCase = TestCase
  { testCaseDescription :: Text.Text
  , testCaseData :: Aeson.Value
  , testCaseValid :: Bool
  }

instance Aeson.FromJSON TestCase where
  parseJSON = Aeson.withObject "TestCase" $ \obj -> do
    description <- obj Aeson..: Key.fromText (Text.pack "description")
    testData <- obj Aeson..: Key.fromText (Text.pack "data")
    valid <- obj Aeson..: Key.fromText (Text.pack "valid")
    pure TestCase{testCaseDescription = description, testCaseData = testData, testCaseValid = valid}

skippedSuiteGroups :: Map.Map (String, Text.Text) Text.Text
skippedSuiteGroups =
  Map.fromList
    [
      ( ("pattern", Text.pack "pattern with Unicode property escape requires unicode mode")
      , Text.pack "regex-tdfa has no unicode property escapes"
      )
    ]

mkTestCase :: Validate.Registry -> Schema.Schema -> TestCase -> Tasty.TestTree
mkTestCase registry schema tc =
  HUnit.testCase (Text.unpack . testCaseDescription $ tc) $ do
    let
      errors = Validate.validate (Validate.compile registry schema) . testCaseData $ tc
      actual = null errors
    HUnit.assertEqual
      (if testCaseValid tc then "expected valid" else "expected invalid, got errors: " <> show errors)
      (testCaseValid tc)
      actual

loadOptionalTestSuite :: Validate.Registry -> IO [Tasty.TestTree]
loadOptionalTestSuite registry = do
  general <- traverse (loadSuiteFile registry) optionalSuiteFiles
  formatSuites <-
    traverse
      (loadSuiteFile (Validate.withFormatAssertion Validate.FormatAssertionAlways registry))
      optionalFormatSuiteFiles
  pure (general <> formatSuites)

propTrueAcceptsAll :: H.Property
propTrueAcceptsAll = H.property $ do
  value <- H.forAll SchemaGen.genJsonValue
  ok <- validatesProp (Schema.BoolSchema True) value
  H.assert ok

validatesProp :: Schema.Schema -> Aeson.Value -> H.PropertyT IO Bool
validatesProp = TestUtil.validatesProp

propFalseRejectsAll :: H.Property
propFalseRejectsAll = H.property $ do
  value <- H.forAll SchemaGen.genJsonValue
  ok <- validatesProp (Schema.BoolSchema False) value
  H.assert (not ok)

propEmptyAcceptsAll :: H.Property
propEmptyAcceptsAll = H.property $ do
  value <- H.forAll SchemaGen.genJsonValue
  ok <- validatesProp (Schema.ObjectSchema Schema.emptySchema) value
  H.assert ok

testNullType :: HUnit.Assertion
testNullType =
  assertValidates "null validates as null" (typeSchema Schema.TypeNull) Aeson.Null

assertValidates :: String -> Schema.Schema -> Aeson.Value -> HUnit.Assertion
assertValidates = TestUtil.assertValidates

typeSchema :: Schema.SchemaType -> Schema.Schema
typeSchema ty = Schema.ObjectSchema Schema.emptySchema{Schema.schemaType = Just (NEL.singleton ty)}

propBoolType :: H.Property
propBoolType = H.property $ do
  b <- H.forAll Gen.bool
  ok <- validatesProp (typeSchema Schema.TypeBoolean) (Aeson.Bool b)
  H.assert ok

propStringType :: H.Property
propStringType = H.property $ do
  s <- H.forAll SchemaGen.genText
  ok <- validatesProp (typeSchema Schema.TypeString) (Aeson.String s)
  H.assert ok

propNumberType :: H.Property
propNumberType = H.property $ do
  n <- H.forAll SchemaGen.genScientific
  ok <- validatesProp (typeSchema Schema.TypeNumber) (Aeson.Number n)
  H.assert ok

propIntegerType :: H.Property
propIntegerType = H.property $ do
  n <-
    H.forAll
      $ Gen.frequency
        [ (8, Gen.int (Range.linear (-1000) 1000))
        , (1, Gen.element [minBound, maxBound, minBound + 1, maxBound - 1])
        ]
  ok <- validatesProp (typeSchema Schema.TypeInteger) (Aeson.Number (fromIntegral n))
  H.assert ok

testIntegerAcceptsOnePointZero :: HUnit.Assertion
testIntegerAcceptsOnePointZero =
  assertValidates
    "type: integer must accept 1.0 because Scientific.isInteger 1.0 == True"
    (typeSchema Schema.TypeInteger)
    (Aeson.Number (Scientific.fromFloatDigits (1.0 :: Double)))

testIntegerRejectsOnePointFive :: HUnit.Assertion
testIntegerRejectsOnePointFive =
  assertRejects
    "type: integer must reject 1.5"
    (typeSchema Schema.TypeInteger)
    (Aeson.Number (Scientific.fromFloatDigits (1.5 :: Double)))

assertRejects :: String -> Schema.Schema -> Aeson.Value -> HUnit.Assertion
assertRejects = TestUtil.assertRejects

propArrayType :: H.Property
propArrayType = H.property $ do
  arr <- H.forAll SchemaGen.genJsonArray
  ok <- validatesProp (typeSchema Schema.TypeArray) (Aeson.Array arr)
  H.assert ok

propObjectType :: H.Property
propObjectType = H.property $ do
  obj <- H.forAll SchemaGen.genJsonObject
  ok <- validatesProp (typeSchema Schema.TypeObject) (Aeson.Object obj)
  H.assert ok

testStringRejectsNumber :: HUnit.Assertion
testStringRejectsNumber = assertTypeMismatch Schema.TypeString (Aeson.Number 42)

assertTypeMismatch :: Schema.SchemaType -> Aeson.Value -> HUnit.Assertion
assertTypeMismatch ty value = do
  let
    errors = TestUtil.validatePure (typeSchema ty) value
  assertSingleError errors $ \err ->
    case Validate.errorKind err of
      Validate.TypeMismatch types actual -> do
        HUnit.assertEqual ("expected types for input " <> show value) (NEL.singleton ty) types
        HUnit.assertEqual ("offending value for input " <> show value) value actual
      other ->
        HUnit.assertFailure
          ( "expected TypeMismatch ["
              <> show ty
              <> "] for input "
              <> show value
              <> ", got: "
              <> show other
          )

assertSingleError ::
  (Foldable t) =>
  t Validate.ValidationError -> (Validate.ValidationError -> HUnit.Assertion) -> HUnit.Assertion
assertSingleError errors k = case Foldable.toList errors of
  [e] -> k e
  xs -> HUnit.assertFailure ("expected 1 error, got " <> show (length xs) <> ": " <> show xs)

testNumberRejectsString :: HUnit.Assertion
testNumberRejectsString = assertTypeMismatch Schema.TypeNumber (Aeson.String (Text.pack "hello"))

testBooleanRejectsNull :: HUnit.Assertion
testBooleanRejectsNull = assertTypeMismatch Schema.TypeBoolean Aeson.Null

testNullRejectsBool :: HUnit.Assertion
testNullRejectsBool = assertTypeMismatch Schema.TypeNull (Aeson.Bool True)

testArrayRejectsObject :: HUnit.Assertion
testArrayRejectsObject = assertTypeMismatch Schema.TypeArray (Aeson.Object KeyMap.empty)

testObjectRejectsArray :: HUnit.Assertion
testObjectRejectsArray = assertTypeMismatch Schema.TypeObject (Aeson.Array Vector.empty)

propMinimum :: H.Property
propMinimum = H.property $ do
  bound <- H.forAll SchemaGen.genScientific
  n <- H.forAll SchemaGen.genScientific
  let
    schema = Schema.ObjectSchema Schema.emptySchema{Schema.schemaMinimum = Just bound}
  ok <- validatesProp schema (Aeson.Number n)
  ok H.=== (n >= bound)

propMaximum :: H.Property
propMaximum = H.property $ do
  bound <- H.forAll SchemaGen.genScientific
  n <- H.forAll SchemaGen.genScientific
  let
    schema = Schema.ObjectSchema Schema.emptySchema{Schema.schemaMaximum = Just bound}
  ok <- validatesProp schema (Aeson.Number n)
  ok H.=== (n <= bound)

propExclusiveMinimum :: H.Property
propExclusiveMinimum = H.property $ do
  bound <- H.forAll SchemaGen.genScientific
  n <- H.forAll SchemaGen.genScientific
  let
    schema = Schema.ObjectSchema Schema.emptySchema{Schema.schemaExclusiveMinimum = Just bound}
  ok <- validatesProp schema (Aeson.Number n)
  ok H.=== (n > bound)

propExclusiveMaximum :: H.Property
propExclusiveMaximum = H.property $ do
  bound <- H.forAll SchemaGen.genScientific
  n <- H.forAll SchemaGen.genScientific
  let
    schema = Schema.ObjectSchema Schema.emptySchema{Schema.schemaExclusiveMaximum = Just bound}
  ok <- validatesProp schema (Aeson.Number n)
  ok H.=== (n < bound)

propMinLength :: H.Property
propMinLength = H.property $ do
  minLen <- H.forAll $ Gen.word (Range.linear 0 20)
  s <- H.forAll SchemaGen.genUnicodeText
  let
    schema = Schema.ObjectSchema Schema.emptySchema{Schema.schemaMinLength = Just minLen}
  ok <- validatesProp schema (Aeson.String s)
  ok H.=== (fromIntegral (Text.length s) >= minLen)

propMaxLength :: H.Property
propMaxLength = H.property $ do
  maxLen <- H.forAll $ Gen.word (Range.linear 0 20)
  s <- H.forAll SchemaGen.genUnicodeText
  let
    schema = Schema.ObjectSchema Schema.emptySchema{Schema.schemaMaxLength = Just maxLen}
  ok <- validatesProp schema (Aeson.String s)
  ok H.=== (fromIntegral (Text.length s) <= maxLen)

propPatternUnicode :: H.Property
propPatternUnicode = H.property $ do
  digits <- H.forAll . Gen.list (Range.linear 1 10) $ Gen.element ['0' .. '9']
  unicode <- H.forAll SchemaGen.genUnicodeText
  let
    schema =
      Schema.ObjectSchema Schema.emptySchema{Schema.schemaPattern = Just (Text.pack "^[0-9]+$")}
    digitText = Text.pack digits
  digitOk <- validatesProp schema (Aeson.String digitText)
  H.assert digitOk
  let
    combined = digitText <> unicode
    allDigit = Text.all Char.isDigit combined && not (Text.null combined)
  combinedOk <- validatesProp schema (Aeson.String combined)
  combinedOk H.=== allDigit

propMinItems :: H.Property
propMinItems = H.property $ do
  minN <- H.forAll $ Gen.word (Range.linear 0 10)
  arr <- H.forAll SchemaGen.genJsonArray
  let
    schema = Schema.ObjectSchema Schema.emptySchema{Schema.schemaMinItems = Just minN}
  ok <- validatesProp schema (Aeson.Array arr)
  ok H.=== (fromIntegral (Vector.length arr) >= minN)

propMaxItems :: H.Property
propMaxItems = H.property $ do
  maxN <- H.forAll $ Gen.word (Range.linear 0 10)
  arr <- H.forAll SchemaGen.genJsonArray
  let
    schema = Schema.ObjectSchema Schema.emptySchema{Schema.schemaMaxItems = Just maxN}
  ok <- validatesProp schema (Aeson.Array arr)
  ok H.=== (fromIntegral (Vector.length arr) <= maxN)

propUniqueItems :: H.Property
propUniqueItems = H.property $ do
  arr <- H.forAll SchemaGen.genJsonArray
  let
    schema = Schema.ObjectSchema Schema.emptySchema{Schema.schemaUniqueItems = Just True}
    elems = Foldable.toList arr
    distinct = length elems == HashSet.size (HashSet.fromList elems)
  accepted <- validatesProp schema (Aeson.Array arr)
  accepted H.=== distinct

propMinContainsRejectsBelow :: H.Property
propMinContainsRejectsBelow = H.property $ do
  k <- H.forAll $ Gen.word (Range.linear 0 5)
  elems <- H.forAll $ Gen.list (Range.linear 0 8) genContainsElement
  let
    inner = typeSchema Schema.TypeNumber
    schema =
      Schema.ObjectSchema
        Schema.emptySchema
          { Schema.schemaContains = Just inner
          , Schema.schemaMinContains = Just k
          }
    matches = length . filter isNumberValue $ elems
    arr = Aeson.Array . Vector.fromList $ elems
  ok <- validatesProp schema arr
  ok H.=== (fromIntegral matches >= k)

genContainsElement :: H.Gen Aeson.Value
genContainsElement =
  Gen.choice
    [ Aeson.Number <$> SchemaGen.genScientific
    , Aeson.Bool <$> Gen.bool
    ]

isNumberValue :: Aeson.Value -> Bool
isNumberValue v = case v of
  Aeson.Number _ -> True
  _ -> False

propMaxContainsRejectsAbove :: H.Property
propMaxContainsRejectsAbove = H.property $ do
  k <- H.forAll $ Gen.word (Range.linear 0 5)
  elems <- H.forAll $ Gen.list (Range.linear 0 8) genContainsElement
  let
    inner = typeSchema Schema.TypeNumber
    schema =
      Schema.ObjectSchema
        Schema.emptySchema
          { Schema.schemaContains = Just inner
          , Schema.schemaMaxContains = Just k
          }
    matches = length . filter isNumberValue $ elems
    arr = Aeson.Array . Vector.fromList $ elems
    upperOk = fromIntegral matches <= k
    lowerOk = matches >= 1
  ok <- validatesProp schema arr
  ok H.=== (upperOk && lowerOk)

propMinZeroPermitsEmpty :: H.Property
propMinZeroPermitsEmpty = H.property $ do
  bools <- H.forAll $ Gen.list (Range.linear 0 5) Gen.bool
  let
    inner = typeSchema Schema.TypeNumber
    schema =
      Schema.ObjectSchema
        Schema.emptySchema
          { Schema.schemaContains = Just inner
          , Schema.schemaMinContains = Just 0
          }
    arr = Aeson.Array . Vector.fromList . fmap Aeson.Bool $ bools
  ok <- validatesProp schema arr
  H.assert ok

testBareContainsRejectsZero :: HUnit.Assertion
testBareContainsRejectsZero =
  let
    schema =
      Schema.ObjectSchema
        Schema.emptySchema
          { Schema.schemaContains = Just (typeSchema Schema.TypeNumber)
          }
    arr = Aeson.Array (Vector.fromList [Aeson.Bool True, Aeson.String (Text.pack "x")])
    errors = TestUtil.validatePure schema arr
    isContainsFailed e = case Validate.errorKind e of
      Validate.ContainsFailed -> True
      _ -> False
   in
    HUnit.assertBool
      ("expected ContainsFailed, got: " <> show errors)
      (any isContainsFailed errors)

testBareContainsAcceptsOne :: HUnit.Assertion
testBareContainsAcceptsOne =
  let
    schema =
      Schema.ObjectSchema
        Schema.emptySchema
          { Schema.schemaContains = Just (typeSchema Schema.TypeNumber)
          }
    arr = Aeson.Array (Vector.fromList [Aeson.Bool True, Aeson.Number 1])
   in
    assertValidates "expected validation success" schema arr

propUnevaluatedItemsSeesPrefixItems :: H.Property
propUnevaluatedItemsSeesPrefixItems = H.property $ do
  n <- H.forAll $ Gen.int (Range.linear 0 3)
  items <- H.forAll $ Gen.list (Range.linear 0 6) SchemaGen.genJsonValue
  let
    schema =
      Schema.ObjectSchema
        Schema.emptySchema
          { Schema.schemaPrefixItems = NEL.nonEmpty (replicate n (Schema.BoolSchema True))
          , Schema.schemaUnevaluatedItems = Just (Schema.BoolSchema False)
          }
    value = Aeson.Array (Vector.fromList items)
    errors = TestUtil.validatePure schema value
  if length items <= n
    then H.assert (null errors)
    else do
      H.assert (not (null errors))
      H.assert (all isUnevaluatedItemsFalseError errors)

isUnevaluatedItemsFalseError :: Validate.ValidationError -> Bool
isUnevaluatedItemsFalseError e =
  case Validate.errorKind e of
    Validate.FalseSchemaRejection -> Text.pack "unevaluatedItems" `elem` Validate.errorSchemaPath e
    _ -> False

propUnevaluatedItemsSeesItems :: H.Property
propUnevaluatedItemsSeesItems = H.property $ do
  items <- H.forAll $ Gen.list (Range.linear 0 6) SchemaGen.genJsonValue
  let
    schema =
      Schema.ObjectSchema
        Schema.emptySchema
          { Schema.schemaItems = Just (Schema.BoolSchema True)
          , Schema.schemaUnevaluatedItems = Just (Schema.BoolSchema False)
          }
  ok <- validatesProp schema (Aeson.Array (Vector.fromList items))
  H.assert ok

propUnevaluatedItemsSeesContains :: H.Property
propUnevaluatedItemsSeesContains = H.property $ do
  x <- H.forAll SchemaGen.genJsonValue
  matching <- H.forAll . Gen.list (Range.linear 1 5) $ pure x
  nonMatching <- H.forAll . Gen.list (Range.linear 1 4) $ Gen.filter (/= x) SchemaGen.genJsonValue
  let
    schema =
      Schema.ObjectSchema
        Schema.emptySchema
          { Schema.schemaContains = Just (Schema.ObjectSchema Schema.emptySchema{Schema.schemaConst = Just x})
          , Schema.schemaUnevaluatedItems = Just (Schema.BoolSchema False)
          }
    toArr = Aeson.Array . Vector.fromList
  allMatchOk <- validatesProp schema (toArr matching)
  H.assert allMatchOk
  mixedOk <- validatesProp schema (toArr (x : nonMatching))
  H.assert (not mixedOk)

propUnevaluatedItemsIgnoresNonArrays :: H.Property
propUnevaluatedItemsIgnoresNonArrays = H.property $ do
  value <- H.forAll $ Gen.filter (not . isArrayValue) SchemaGen.genJsonValue
  let
    schema =
      Schema.ObjectSchema
        Schema.emptySchema{Schema.schemaUnevaluatedItems = Just (Schema.BoolSchema False)}
  ok <- validatesProp schema value
  H.assert ok
  where
    isArrayValue v = case v of
      Aeson.Array _ -> True
      _ -> False

propUnevaluatedItemsAcrossAllOf :: H.Property
propUnevaluatedItemsAcrossAllOf = H.property $ do
  items <- H.forAll $ Gen.list (Range.linear 0 6) SchemaGen.genJsonValue
  let
    accept = Schema.BoolSchema True
    branch =
      Schema.ObjectSchema
        Schema.emptySchema{Schema.schemaPrefixItems = Just (accept NEL.:| [accept])}
    schema =
      Schema.ObjectSchema
        Schema.emptySchema
          { Schema.schemaPrefixItems = Just (NEL.singleton accept)
          , Schema.schemaAllOf = Just (NEL.singleton branch)
          , Schema.schemaUnevaluatedItems = Just (Schema.BoolSchema False)
          }
    errors = TestUtil.validatePure schema (Aeson.Array (Vector.fromList items))
  if length items <= 2
    then H.assert (null errors)
    else do
      H.assert (not (null errors))
      H.assert (all isUnevaluatedItemsFalseError errors)

propUnevaluatedItemsInstanceLocation :: H.Property
propUnevaluatedItemsInstanceLocation = H.property $ do
  a <- H.forAll SchemaGen.genJsonValue
  b <- H.forAll SchemaGen.genJsonValue
  c <- H.forAll SchemaGen.genJsonValue
  let
    accept = Schema.BoolSchema True
    inner =
      Schema.ObjectSchema
        Schema.emptySchema{Schema.schemaPrefixItems = Just (accept NEL.:| [accept])}
    schema =
      Schema.ObjectSchema
        Schema.emptySchema
          { Schema.schemaPrefixItems = Just (NEL.singleton inner)
          , Schema.schemaUnevaluatedItems = Just (Schema.BoolSchema False)
          }
    value = Aeson.Array (Vector.fromList [Aeson.Array (Vector.fromList [a, b]), c])
    errors = TestUtil.validatePure schema value
  H.assert (not (null errors))
  H.assert (all isUnevaluatedItemsFalseError errors)

propUnevaluatedItemsCodecRoundTrip :: H.Property
propUnevaluatedItemsCodecRoundTrip = H.property $ do
  sub <- H.forAll SchemaGen.genSchema
  let
    schema =
      Schema.ObjectSchema
        Schema.emptySchema{Schema.schemaUnevaluatedItems = Just sub}
    encoded = Aeson.encode schema
  decoded <- case TestUtil.decodeSchema encoded of
    Left err -> H.footnote err *> H.failure
    Right roundtripped -> pure roundtripped
  case decoded of
    Schema.ObjectSchema obj -> Schema.schemaUnevaluatedItems obj H.=== Just sub
    Schema.BoolSchema _ -> H.footnote "decoded as BoolSchema" *> H.failure

propRequiredPresent :: H.Property
propRequiredPresent = H.property $ do
  key <- H.forAll SchemaGen.genFieldName
  value <- H.forAll SchemaGen.genJsonValue
  let
    schema = Schema.ObjectSchema Schema.emptySchema{Schema.schemaRequired = Just (OSet.singleton key)}
    obj = KeyMap.singleton (Key.fromText key) value
  ok <- validatesProp schema (Aeson.Object obj)
  H.assert ok

testRequiredMissing :: HUnit.Assertion
testRequiredMissing =
  let
    schema = Schema.ObjectSchema Schema.emptySchema{Schema.schemaRequired = Just (OSet.singleton (Text.pack "missing"))}
   in
    assertRejects "missing required property rejected" schema (Aeson.Object KeyMap.empty)

propAdditionalPropertiesFalse :: H.Property
propAdditionalPropertiesFalse = H.property $ do
  knownKeys <- H.forAll . Gen.set (Range.linear 0 4) $ SchemaGen.genFieldName
  obj <- H.forAll SchemaGen.genJsonObject
  let
    propsMap = OMap.fromList . fmap (\k -> (k, Schema.BoolSchema True)) . Set.toList $ knownKeys
    schema =
      Schema.ObjectSchema
        Schema.emptySchema
          { Schema.schemaProperties = Just propsMap
          , Schema.schemaAdditionalProperties = Just . Schema.BoolSchema $ False
          }
    objKeys = fmap Key.toText . KeyMap.keys $ obj
    allKnown = all (`Set.member` knownKeys) objKeys
  accepted <- validatesProp schema (Aeson.Object obj)
  accepted H.=== allKnown

propAllOf :: H.Property
propAllOf = H.property $ do
  subs <- H.forAll . Gen.nonEmpty (Range.linear 1 4) $ SchemaGen.genSchema
  value <- H.forAll SchemaGen.genJsonValue
  let
    schema = Schema.ObjectSchema Schema.emptySchema{Schema.schemaAllOf = Just subs}
  subResults <- traverse (`validatesProp` value) subs
  ok <- validatesProp schema value
  ok H.=== and subResults

propAnyOf :: H.Property
propAnyOf = H.property $ do
  subs <- H.forAll . Gen.nonEmpty (Range.linear 1 4) $ SchemaGen.genSchema
  value <- H.forAll SchemaGen.genJsonValue
  let
    schema = Schema.ObjectSchema Schema.emptySchema{Schema.schemaAnyOf = Just subs}
  subResults <- traverse (`validatesProp` value) subs
  ok <- validatesProp schema value
  ok H.=== or subResults

propNot :: H.Property
propNot = H.property $ do
  value <- H.forAll SchemaGen.genJsonValue
  let
    schema = Schema.ObjectSchema Schema.emptySchema{Schema.schemaNot = Just . typeSchema $ Schema.TypeString}
  ok <- validatesProp schema value
  case value of
    Aeson.String _ -> H.assert (not ok)
    _ -> H.assert ok

propIfThenElse :: H.Property
propIfThenElse = H.property $ do
  ifS <- H.forAll SchemaGen.genSchema
  thenS <- H.forAll SchemaGen.genSchema
  elseS <- H.forAll SchemaGen.genSchema
  value <- H.forAll SchemaGen.genJsonValue
  let
    schema =
      Schema.ObjectSchema
        Schema.emptySchema
          { Schema.schemaIf = Just ifS
          , Schema.schemaThen = Just thenS
          , Schema.schemaElse = Just elseS
          }
  ifOk <- validatesProp ifS value
  branchPasses <-
    if ifOk
      then validatesProp thenS value
      else validatesProp elseS value
  ok <- validatesProp schema value
  ok H.=== branchPasses

propConst :: H.Property
propConst = H.property $ do
  value <- H.forAll SchemaGen.genJsonValue
  let
    schema = Schema.ObjectSchema Schema.emptySchema{Schema.schemaConst = Just value}
  ok <- validatesProp schema value
  H.assert ok

propEnum :: H.Property
propEnum = H.property $ do
  first <- H.forAll SchemaGen.genJsonValue
  rest <- H.forAll $ Gen.list (Range.linear 0 4) SchemaGen.genJsonValue
  let
    values = first : rest
    schema = Schema.ObjectSchema Schema.emptySchema{Schema.schemaEnum = Just values}
  chosen <- H.forAll $ Gen.element values
  ok <- validatesProp schema chosen
  H.assert ok

propParseRoundtrip :: H.Property
propParseRoundtrip = H.property $ do
  schema <- H.forAll SchemaGen.genSchema
  let
    encoded = Aeson.encode schema
  case TestUtil.decodeSchema encoded of
    Left err -> H.footnote err *> H.failure
    Right roundtripped -> roundtripped H.=== schema

propValidateTotal :: H.Property
propValidateTotal = H.property $ do
  schema <- H.forAll SchemaGen.genSchema
  value <- H.forAll SchemaGen.genJsonValue
  _ <- H.evalNF (TestUtil.validatePure schema value)
  H.success

propUnresolvableRef :: H.Property
propUnresolvableRef = H.property $ do
  ref <- H.forAll genUnresolvableRef
  value <- H.forAll SchemaGen.genJsonValue
  let
    schema = Schema.ObjectSchema Schema.emptySchema{Schema.schemaRef = Just (Schema.SchemaRef ref)}
    errors = TestUtil.validatePure schema value
  H.assert (any (TestUtil.isRefUnresolved . Validate.errorKind) errors)

genUnresolvableRef :: H.Gen Text.Text
genUnresolvableRef =
  let
    nonsense = Gen.text (Range.linear 1 8) Gen.alphaNum
    flavours =
      [ fmap (\n -> Text.pack "#/$defs/" <> n) nonsense
      , fmap (\n -> Text.pack "#" <> n) nonsense
      , fmap (\n -> Text.pack "https://no-such-host.invalid/" <> n) nonsense
      ]
   in
    Gen.choice flavours

propDependentRequiredRejects :: H.Property
propDependentRequiredRejects = H.property $ do
  a <- H.forAll SchemaGen.genFieldName
  b <- H.forAll $ Gen.filter (/= a) SchemaGen.genFieldName
  value <- H.forAll SchemaGen.genJsonValue
  let
    schema =
      Schema.ObjectSchema
        Schema.emptySchema
          { Schema.schemaDependencies =
              Just (OMap.fromList [(a, Schema.PropertyDependency [b])])
          }
    obj = Aeson.Object (KeyMap.singleton (Key.fromText a) value)
    errors = TestUtil.validatePure schema obj
    isMatching e = case Validate.errorKind e of
      Validate.DependentRequiredMissing keyword depKey k ->
        keyword == Text.pack "dependencies" && depKey == a && k == b
      _ -> False
  H.assert (any isMatching errors)

propDependentRequiredStandaloneRejects :: H.Property
propDependentRequiredStandaloneRejects = H.property $ do
  a <- H.forAll SchemaGen.genFieldName
  b <- H.forAll $ Gen.filter (/= a) SchemaGen.genFieldName
  value <- H.forAll SchemaGen.genJsonValue
  let
    schema =
      Schema.ObjectSchema
        Schema.emptySchema
          { Schema.schemaDependentRequired = Just (OMap.fromList [(a, [b])])
          }
    obj = Aeson.Object (KeyMap.singleton (Key.fromText a) value)
    errors = TestUtil.validatePure schema obj
    isMatching e = case Validate.errorKind e of
      Validate.DependentRequiredMissing keyword depKey k ->
        keyword == Text.pack "dependentRequired" && depKey == a && k == b
      _ -> False
  H.assert (any isMatching errors)

propDependentRequiredTriggerAbsent :: H.Property
propDependentRequiredTriggerAbsent = H.property $ do
  a <- H.forAll SchemaGen.genFieldName
  b <- H.forAll $ Gen.filter (/= a) SchemaGen.genFieldName
  otherKeys <- H.forAll . Gen.set (Range.linear 0 4) $ Gen.filter (/= a) SchemaGen.genFieldName
  values <- H.forAll . Gen.list (Range.singleton (Set.size otherKeys)) $ SchemaGen.genJsonValue
  let
    schema =
      Schema.ObjectSchema
        Schema.emptySchema
          { Schema.schemaDependentRequired = Just (OMap.fromList [(a, [b])])
          }
    obj =
      Aeson.Object
        . KeyMap.fromList
        $ zip (fmap Key.fromText (Set.toList otherKeys)) values
  ok <- TestUtil.validatesProp schema obj
  H.assert ok

propDependentRequiredIgnoresNonObjects :: H.Property
propDependentRequiredIgnoresNonObjects = H.property $ do
  a <- H.forAll SchemaGen.genFieldName
  b <- H.forAll SchemaGen.genFieldName
  value <- H.forAll $ Gen.filter (not . isJsonObject) SchemaGen.genJsonValue
  let
    schema =
      Schema.ObjectSchema
        Schema.emptySchema
          { Schema.schemaDependentRequired = Just (OMap.fromList [(a, [b])])
          }
  ok <- TestUtil.validatesProp schema value
  H.assert ok
  where
    isJsonObject v = case v of
      Aeson.Object _ -> True
      _ -> False

propDependentRequiredEscapedKeys :: H.Property
propDependentRequiredEscapedKeys = H.property $ do
  let
    genKey =
      Gen.choice
        [ Gen.text (Range.linear 1 6) Gen.alpha
        , Gen.text (Range.linear 1 6) (Gen.element ['"', '\n', '\r', '\\', '\t'])
        ]
  a <- H.forAll genKey
  b <- H.forAll $ Gen.filter (/= a) genKey
  let
    schema =
      Schema.ObjectSchema
        Schema.emptySchema
          { Schema.schemaDependentRequired = Just (OMap.fromList [(a, [b])])
          }
    encoded = Aeson.encode schema
  decoded <- case TestUtil.decodeSchema encoded of
    Left err -> H.footnote err *> H.failure
    Right roundtripped -> pure roundtripped
  let
    obj = Aeson.Object (KeyMap.singleton (Key.fromText a) Aeson.Null)
    errors = TestUtil.validatePure decoded obj
    isMatching e = case Validate.errorKind e of
      Validate.DependentRequiredMissing keyword depKey k ->
        keyword == Text.pack "dependentRequired" && depKey == a && k == b
      _ -> False
  H.assert (any isMatching errors)

propDependentRequiredOneFaultPerMissing :: H.Property
propDependentRequiredOneFaultPerMissing = H.property $ do
  a <- H.forAll SchemaGen.genFieldName
  dependents <- H.forAll . Gen.set (Range.linear 1 5) $ Gen.filter (/= a) SchemaGen.genFieldName
  value <- H.forAll SchemaGen.genJsonValue
  let
    reqKeys = Set.toList dependents
    schema =
      Schema.ObjectSchema
        Schema.emptySchema
          { Schema.schemaDependentRequired = Just (OMap.fromList [(a, reqKeys)])
          }
    obj = Aeson.Object (KeyMap.singleton (Key.fromText a) value)
    errors = TestUtil.validatePure schema obj
    missingKeys =
      Maybe.mapMaybe
        ( \e -> case Validate.errorKind e of
            Validate.DependentRequiredMissing keyword _ k
              | keyword == Text.pack "dependentRequired" -> Just k
            _ -> Nothing
        )
        errors
  missingKeys H.=== reqKeys

propResolveUriEmptyPre :: H.Property
propResolveUriEmptyPre = H.property $ do
  base <- H.forAll . Gen.text (Range.linear 1 10) $ Gen.alphaNum
  relative <- H.forAll . Gen.text (Range.linear 1 10) $ Gen.alphaNum
  Uri.resolveUri (Just base) relative H.=== relative

propHasUniqueItemsLaw :: H.Property
propHasUniqueItemsLaw = H.property $ do
  let
    pool =
      [ Aeson.Null
      , Aeson.Bool True
      , Aeson.Bool False
      , Aeson.Number 0
      , Aeson.Number 1
      , Aeson.Number 2
      , Aeson.String (Text.pack "a")
      , Aeson.String (Text.pack "b")
      ]
  xs <- H.forAll . Gen.list (Range.linear 0 12) . Gen.element $ pool
  let
    expected = HashSet.size (HashSet.fromList xs) == length xs
  Array.hasUniqueItems xs H.=== expected

testMultiDocDynamicRef :: HUnit.Assertion
testMultiDocDynamicRef = do
  let
    bDocId = Text.pack "http://example.invalid/b"
    bAnchorBody =
      Schema.ObjectSchema
        Schema.emptySchema
          { Schema.schemaDynamicAnchor = Just (Schema.DynamicAnchor (Text.pack "x"))
          , Schema.schemaType = Just (NEL.singleton Schema.TypeNumber)
          }
    bMain =
      Schema.ObjectSchema
        Schema.emptySchema
          { Schema.schemaId = Just (Schema.SchemaId bDocId)
          , Schema.schemaType = Just (NEL.singleton Schema.TypeObject)
          , Schema.schemaProperties =
              Just . OMap.fromList
                $ [
                    ( Text.pack "leaf"
                    , Schema.ObjectSchema
                        Schema.emptySchema
                          { Schema.schemaDynamicRef = Just (Schema.DynamicRef (Text.pack "#x"))
                          }
                    )
                  ]
          , Schema.schemaDefs = Just . OMap.fromList $ [(Text.pack "x", bAnchorBody)]
          }
    aDocId = Text.pack "http://example.invalid/a"
    aAnchorBody =
      Schema.ObjectSchema
        Schema.emptySchema
          { Schema.schemaDynamicAnchor = Just (Schema.DynamicAnchor (Text.pack "x"))
          , Schema.schemaType = Just (NEL.singleton Schema.TypeString)
          }
    aMain =
      Schema.ObjectSchema
        Schema.emptySchema
          { Schema.schemaId = Just (Schema.SchemaId aDocId)
          , Schema.schemaRef = Just (Schema.SchemaRef bDocId)
          , Schema.schemaDefs = Just . OMap.fromList $ [(Text.pack "x", aAnchorBody)]
          }
    registry = foldr Validate.addSchema Validate.emptyRegistry [aMain, bMain]
    okValue =
      Aeson.Object
        . KeyMap.singleton (Key.fromText (Text.pack "leaf"))
        $ Aeson.String (Text.pack "ok")
    failValue =
      Aeson.Object
        . KeyMap.singleton (Key.fromText (Text.pack "leaf"))
        $ Aeson.Number 42
    okErrors = Validate.validate (Validate.compile registry aMain) okValue
    failErrors = Validate.validate (Validate.compile registry aMain) failValue
  HUnit.assertBool
    ( "expected leaf:\"ok\" to validate via outermost $dynamicAnchor"
        <> " (type:string in A); got: "
        <> show okErrors
    )
    (null okErrors)
  HUnit.assertBool
    "expected leaf:42 to fail under A's type:string anchor, got no errors"
    (not (null failErrors))

testSearchDefsScopeAnchored :: Bool -> HUnit.Assertion
testSearchDefsScopeAnchored reversed = do
  let
    outerId = Text.pack "http://example.invalid/aaa"
    innerId = Text.pack "http://example.invalid/bbb"
    otherId = Text.pack "http://example.invalid/000"
    outerDoc =
      Schema.ObjectSchema
        Schema.emptySchema
          { Schema.schemaId = Just (Schema.SchemaId outerId)
          , Schema.schemaRef = Just (Schema.SchemaRef innerId)
          , Schema.schemaDefs =
              Just . OMap.fromList
                $ [
                    ( Text.pack "x"
                    , Schema.ObjectSchema
                        Schema.emptySchema
                          { Schema.schemaType = Just (NEL.singleton Schema.TypeString)
                          }
                    )
                  ]
          }
    innerDoc =
      Schema.ObjectSchema
        Schema.emptySchema
          { Schema.schemaId = Just (Schema.SchemaId innerId)
          , Schema.schemaDynamicRef = Just (Schema.DynamicRef (Text.pack "#/$defs/x"))
          }
    otherDoc =
      Schema.ObjectSchema
        Schema.emptySchema
          { Schema.schemaId = Just (Schema.SchemaId otherId)
          , Schema.schemaDefs =
              Just . OMap.fromList
                $ [
                    ( Text.pack "x"
                    , Schema.ObjectSchema
                        Schema.emptySchema
                          { Schema.schemaType = Just (NEL.singleton Schema.TypeNumber)
                          }
                    )
                  ]
          }
    docs =
      if reversed
        then [otherDoc, innerDoc, outerDoc]
        else [outerDoc, innerDoc, otherDoc]
    registry = foldr Validate.addSchema Validate.emptyRegistry docs
    stringValue = Aeson.String (Text.pack "hello")
    numberValue = Aeson.Number 42
    stringErrors = Validate.validate (Validate.compile registry outerDoc) stringValue
    numberErrors = Validate.validate (Validate.compile registry outerDoc) numberValue
  HUnit.assertBool
    ( "expected \"hello\" to validate via /aaa's $defs/x (type:string);"
        <> " reversed="
        <> show reversed
        <> " errors="
        <> show stringErrors
    )
    (null stringErrors)
  HUnit.assertBool
    ( "expected 42 to fail under /aaa's $defs/x (type:string),"
        <> " not be accepted via /000's number-x;"
        <> " reversed="
        <> show reversed
    )
    (not (null numberErrors))

testPatternCompileFailedSurvives :: HUnit.Assertion
testPatternCompileFailedSurvives = do
  let
    badPattern = Text.pack "[invalid"
    schema =
      Schema.ObjectSchema
        Schema.emptySchema
          { Schema.schemaType = Just (NEL.singleton Schema.TypeString)
          , Schema.schemaPattern = Just badPattern
          }
    value = Aeson.String (Text.pack "anything")
    compiled = Validate.compile Validate.emptyRegistry schema
    errors = Validate.validate compiled value
    isPatternCompileFailed e = case Validate.errorKind e of
      Validate.PatternCompileFailed _ pat -> pat == badPattern
      _ -> False
  HUnit.assertBool
    ("expected PatternCompileFailed " <> show badPattern <> " among: " <> show errors)
    (any isPatternCompileFailed errors)

testPatternAnchorsWholeString :: HUnit.Assertion
testPatternAnchorsWholeString =
  let
    schema = Schema.ObjectSchema Schema.emptySchema{Schema.schemaPattern = Just (Text.pack "^a$")}
   in
    do
      assertValidates "single line" schema (Aeson.String (Text.pack "a"))
      assertRejects "embedded newline" schema (Aeson.String (Text.pack "b\na"))
      assertRejects "trailing newline" schema (Aeson.String (Text.pack "a\n"))

testDynamicPointerBesideRef :: HUnit.Assertion
testDynamicPointerBesideRef = assertDynamicPointerBesideRef Nothing (Text.pack "#/properties/x")

assertDynamicPointerBesideRef :: Maybe Text.Text -> Text.Text -> HUnit.Assertion
assertDynamicPointerBesideRef schemaId dynRef =
  let
    rawSchema =
      jsonObject
        $ foldMap (\i -> [(Text.pack "$id", Aeson.String i)]) schemaId
          <> [ (Text.pack "$defs", jsonObject [(Text.pack "s", typeString)])
             ,
               ( Text.pack "properties"
               , jsonObject
                   [ (Text.pack "x", jsonObject [(Text.pack "maxLength", Aeson.Number 1)])
                   ,
                     ( Text.pack "y"
                     , jsonObject [(Text.pack "$ref", Aeson.String (Text.pack "#/$defs/s")), (Text.pack "$dynamicRef", Aeson.String dynRef)]
                     )
                   ]
               )
             ]
    instanceWith v = jsonObject [(Text.pack "y", v)]
   in
    do
      schema <- decodeSchemaOrFail rawSchema
      assertValidates "short string" schema (instanceWith (Aeson.String (Text.pack "a")))
      assertSingleError (TestUtil.validatePure schema (instanceWith (Aeson.String (Text.pack "ab")))) $ \err ->
        HUnit.assertEqual "keyword" (Just (Text.pack "maxLength")) (Validate.errorKeyword err)
      assertSingleError (TestUtil.validatePure schema (instanceWith (Aeson.Number 1))) $ \err ->
        HUnit.assertEqual "keyword" (Just (Text.pack "type")) (Validate.errorKeyword err)

jsonObject :: [(Text.Text, Aeson.Value)] -> Aeson.Value
jsonObject = Aeson.Object . KeyMap.fromList . fmap (\(k, v) -> (Key.fromText k, v))

typeString :: Aeson.Value
typeString = jsonObject [(Text.pack "type", Aeson.String (Text.pack "string"))]

decodeSchemaOrFail :: Aeson.Value -> IO Schema.Schema
decodeSchemaOrFail value = case TestUtil.decodeSchemaValue value of
  Left err -> HUnit.assertFailure ("schema decode failed: " <> show err)
  Right schema -> pure schema

testDynamicUriPointerBesideRef :: HUnit.Assertion
testDynamicUriPointerBesideRef =
  assertDynamicPointerBesideRef
    (Just (Text.pack "http://example.com/root.json"))
    (Text.pack "http://example.com/root.json#/properties/x")

testFalseSchemaPath :: HUnit.Assertion
testFalseSchemaPath =
  assertErrorLocation
    (jsonObject [(Text.pack "additionalProperties", Aeson.Bool False)])
    (jsonObject [(Text.pack "a", Aeson.Number 1)])
    Nothing
    [Text.pack "additionalProperties"]

assertErrorLocation :: Aeson.Value -> Aeson.Value -> Maybe Text.Text -> [Text.Text] -> HUnit.Assertion
assertErrorLocation rawSchema value keyword schemaPath = do
  schema <- decodeSchemaOrFail rawSchema
  assertSingleError (TestUtil.validatePure schema value) $ \err -> do
    HUnit.assertEqual "keyword" keyword (Validate.errorKeyword err)
    HUnit.assertEqual "schema path" schemaPath (Validate.errorSchemaPath err)

testPatternPropertiesCompilePath :: HUnit.Assertion
testPatternPropertiesCompilePath =
  assertErrorLocation
    (jsonObject [(Text.pack "patternProperties", jsonObject [(Text.pack "(", Aeson.Bool True)])])
    (jsonObject [(Text.pack "a", Aeson.Number 1)])
    (Just (Text.pack "patternProperties"))
    [Text.pack "patternProperties"]

testDynamicRefUnresolvedKeyword :: HUnit.Assertion
testDynamicRefUnresolvedKeyword =
  assertErrorLocation
    (dynamicRefTo "#nope")
    (Aeson.Number 1)
    (Just (Text.pack "$dynamicRef"))
    [Text.pack "$dynamicRef"]

dynamicRefTo :: String -> Aeson.Value
dynamicRefTo target = jsonObject [(Text.pack "$dynamicRef", Aeson.String (Text.pack target))]

testPatternPropertiesPath :: HUnit.Assertion
testPatternPropertiesPath =
  assertErrorLocation
    (jsonObject [(Text.pack "patternProperties", jsonObject [(Text.pack "^a", typeString)])])
    (jsonObject [(Text.pack "ab", Aeson.Number 1)])
    (Just (Text.pack "type"))
    (fmap Text.pack ["patternProperties", "^a", "type"])

testDependentSchemasPath :: HUnit.Assertion
testDependentSchemasPath =
  assertErrorLocation
    (jsonObject [(Text.pack "dependentSchemas", jsonObject [(Text.pack "a", typeString)])])
    (jsonObject [(Text.pack "a", Aeson.Number 1)])
    (Just (Text.pack "type"))
    (fmap Text.pack ["dependentSchemas", "a", "type"])

testRefCyclePath :: HUnit.Assertion
testRefCyclePath =
  assertErrorLocation (refTo "#") (Aeson.Number 1) (Just (Text.pack "$ref")) [Text.pack "$ref"]

refTo :: String -> Aeson.Value
refTo target = jsonObject [(Text.pack "$ref", Aeson.String (Text.pack target))]

testDynamicRefTargetPath :: HUnit.Assertion
testDynamicRefTargetPath =
  assertErrorLocation
    (jsonObject [nodeDefs typeString, (Text.pack "$dynamicRef", Aeson.String (Text.pack "#node"))])
    (Aeson.Number 1)
    (Just (Text.pack "type"))
    (fmap Text.pack ["$dynamicRef", "type"])

nodeDefs :: Aeson.Value -> (Text.Text, Aeson.Value)
nodeDefs extra =
  ( Text.pack "$defs"
  , jsonObject
      [(Text.pack "n", mergeObjects (jsonObject [(Text.pack "$dynamicAnchor", Aeson.String (Text.pack "node"))]) extra)]
  )

mergeObjects :: Aeson.Value -> Aeson.Value -> Aeson.Value
mergeObjects left right = case (left, right) of
  (Aeson.Object l, Aeson.Object r) -> Aeson.Object (KeyMap.union l r)
  _ -> left

testContainsDescends :: HUnit.Assertion
testContainsDescends = do
  schema <-
    decodeSchemaOrFail
      $ jsonObject
        [ (Text.pack "$dynamicAnchor", Aeson.String (Text.pack "n"))
        , (Text.pack "type", Aeson.toJSON [Text.pack "array", Text.pack "integer"])
        , (Text.pack "contains", dynamicRefTo "#n")
        ]
  assertValidates
    "nested integer"
    schema
    (Aeson.Array (Vector.singleton (Aeson.Array (Vector.singleton (Aeson.Number 1)))))
  assertRejects
    "nested string"
    schema
    (Aeson.Array (Vector.singleton (Aeson.Array (Vector.singleton (Aeson.String (Text.pack "x"))))))

testPropertyNamesDescends :: HUnit.Assertion
testPropertyNamesDescends = do
  schema <-
    decodeSchemaOrFail
      $ jsonObject
        [ (Text.pack "$dynamicRef", Aeson.String (Text.pack "#node"))
        , nodeDefs
            ( jsonObject
                [ (Text.pack "type", Aeson.toJSON [Text.pack "object", Text.pack "string"])
                , (Text.pack "maxLength", Aeson.Number 2)
                , (Text.pack "propertyNames", dynamicRefTo "#node")
                ]
            )
        ]
  assertValidates "short name" schema (jsonObject [(Text.pack "ab", Aeson.Number 1)])
  assertSingleError (TestUtil.validatePure schema (jsonObject [(Text.pack "abc", Aeson.Number 1)])) $ \err -> do
    HUnit.assertEqual "instance path" [Text.pack "abc"] (Validate.errorInstancePath err)
    HUnit.assertEqual "keyword" (Just (Text.pack "maxLength")) (Validate.errorKeyword err)

propResolvedRefLocator :: H.Property
propResolvedRefLocator = H.property $ do
  propName <- H.forAll SchemaGen.genFieldName
  let
    targetId = Text.pack "http://example.invalid/locator-target"
    target =
      Schema.ObjectSchema
        Schema.emptySchema
          { Schema.schemaId = Just (Schema.SchemaId targetId)
          , Schema.schemaType = Just (NEL.singleton Schema.TypeNumber)
          }
    wrapping =
      Schema.ObjectSchema
        Schema.emptySchema
          { Schema.schemaProperties =
              Just . OMap.fromList
                $ [
                    ( propName
                    , Schema.ObjectSchema
                        Schema.emptySchema
                          { Schema.schemaRef = Just (Schema.SchemaRef targetId)
                          }
                    )
                  ]
          }
    extraRegistry = foldr Validate.addSchema Validate.emptyRegistry [target]
    value =
      Aeson.Object
        . KeyMap.singleton (Key.fromText propName)
        $ Aeson.String (Text.pack "not-a-number")
    errors = Validate.validate (Validate.compile extraRegistry wrapping) value
  H.annotateShow errors
  H.assert (not (null errors))
  Foldable.for_ errors $ \err ->
    Validate.errorInstancePath err H.=== [propName]

testRootSelfRefTypeObjectRejectsArray :: HUnit.Assertion
testRootSelfRefTypeObjectRejectsArray = do
  let
    schema =
      Schema.ObjectSchema
        Schema.emptySchema
          { Schema.schemaRef = Just (Schema.SchemaRef (Text.pack "#"))
          , Schema.schemaType = Just (NEL.singleton Schema.TypeObject)
          }
    arr = Aeson.Array Vector.empty
    errors = TestUtil.validatePure schema arr
  HUnit.assertBool
    ("expected root {$ref:\"#\",type:object} to reject array; errors=" <> show errors)
    (not (null errors))

testRootSelfRefTypeStringRejectsNumber :: HUnit.Assertion
testRootSelfRefTypeStringRejectsNumber = do
  let
    schema =
      Schema.ObjectSchema
        Schema.emptySchema
          { Schema.schemaRef = Just (Schema.SchemaRef (Text.pack "#"))
          , Schema.schemaType = Just (NEL.singleton Schema.TypeString)
          }
    num = Aeson.Number 42
    errors = TestUtil.validatePure schema num
  HUnit.assertBool
    ("expected root {$ref:\"#\",type:string} to reject number; errors=" <> show errors)
    (not (null errors))

propRootSelfRefSiblingTypeEnforced :: H.Property
propRootSelfRefSiblingTypeEnforced = H.property $ do
  ty <- H.forAll SchemaGen.genSchemaType
  value <- H.forAll SchemaGen.genJsonValue
  let
    siblingSchema =
      Schema.ObjectSchema
        Schema.emptySchema
          { Schema.schemaRef = Just (Schema.SchemaRef (Text.pack "#"))
          , Schema.schemaType = Just (NEL.singleton ty)
          }
    bareSchema = typeSchema ty
    siblingErrors = TestUtil.validatePure siblingSchema value
  H.assert (any (TestUtil.isRefCycle . Validate.errorKind) siblingErrors)
  all (TestUtil.isRefCycle . Validate.errorKind) siblingErrors
    H.=== null (TestUtil.validatePure bareSchema value)

testDefsRefSiblingMinLengthEnforced :: HUnit.Assertion
testDefsRefSiblingMinLengthEnforced = do
  let
    schema =
      Schema.ObjectSchema
        Schema.emptySchema
          { Schema.schemaRef = Just (Schema.SchemaRef (Text.pack "#/$defs/x"))
          , Schema.schemaMinLength = Just 5
          , Schema.schemaDefs =
              Just (OMap.fromList [(Text.pack "x", typeSchema Schema.TypeString)])
          }
    shortString = Aeson.String (Text.pack "abc")
    errors = TestUtil.validatePure schema shortString
  HUnit.assertBool
    ("expected {$ref:#/$defs/x,minLength:5} to reject \"abc\"; errors=" <> show errors)
    (not (null errors))

testDefsRefSiblingMinimumEnforced :: HUnit.Assertion
testDefsRefSiblingMinimumEnforced = do
  let
    schema =
      Schema.ObjectSchema
        Schema.emptySchema
          { Schema.schemaRef = Just (Schema.SchemaRef (Text.pack "#/$defs/n"))
          , Schema.schemaMinimum = Just 10
          , Schema.schemaDefs =
              Just (OMap.fromList [(Text.pack "n", typeSchema Schema.TypeInteger)])
          }
    small = Aeson.Number 3
    errors = TestUtil.validatePure schema small
  HUnit.assertBool
    ("expected {$ref:#/$defs/n,minimum:10} to reject 3; errors=" <> show errors)
    (not (null errors))

testDefsRefSiblingViaPropertyEnforced :: HUnit.Assertion
testDefsRefSiblingViaPropertyEnforced = do
  let
    refNode =
      Schema.ObjectSchema
        Schema.emptySchema
          { Schema.schemaRef = Just (Schema.SchemaRef (Text.pack "#/$defs/x"))
          , Schema.schemaMinLength = Just 5
          }
    schema =
      Schema.ObjectSchema
        Schema.emptySchema
          { Schema.schemaDefs =
              Just (OMap.fromList [(Text.pack "x", typeSchema Schema.TypeString)])
          , Schema.schemaProperties = Just (OMap.fromList [(Text.pack "p", refNode)])
          }
    inst = Aeson.object [(Key.fromText (Text.pack "p"), Aeson.String (Text.pack "abc"))]
    errors = TestUtil.validatePure schema inst
  HUnit.assertBool
    ("expected property {$ref:#/$defs/x,minLength:5} to reject \"abc\"; errors=" <> show errors)
    (not (null errors))

testResolveUriPassesAbsoluteSchemes :: HUnit.Assertion
testResolveUriPassesAbsoluteSchemes = do
  let
    base = Just (Text.pack "http://example.com/root.json")
  HUnit.assertEqual
    "urn: passes through unchanged"
    (Text.pack "urn:example:target")
    (Uri.resolveUri base (Text.pack "urn:example:target"))
  HUnit.assertEqual
    "file: passes through unchanged"
    (Text.pack "file:///x/y.json")
    (Uri.resolveUri base (Text.pack "file:///x/y.json"))

testNonHttpAbsoluteRefResolves :: HUnit.Assertion
testNonHttpAbsoluteRefResolves = do
  let
    target =
      Schema.ObjectSchema
        Schema.emptySchema
          { Schema.schemaId = Just (Schema.SchemaId (Text.pack "urn:example:target"))
          , Schema.schemaType = Just (NEL.singleton Schema.TypeString)
          }
    holder =
      Schema.ObjectSchema
        Schema.emptySchema
          { Schema.schemaId = Just (Schema.SchemaId (Text.pack "http://example.com/b/holder.json"))
          , Schema.schemaDefs = Just (OMap.fromList [(Text.pack "t", target)])
          }
    refNode =
      Schema.ObjectSchema
        Schema.emptySchema
          { Schema.schemaRef = Just (Schema.SchemaRef (Text.pack "urn:example:target"))
          }
    schema =
      Schema.ObjectSchema
        Schema.emptySchema
          { Schema.schemaId = Just (Schema.SchemaId (Text.pack "http://example.com/a/root.json"))
          , Schema.schemaAllOf = Just (NEL.singleton holder)
          , Schema.schemaProperties = Just (OMap.fromList [(Text.pack "foo", refNode)])
          }
    mkInst v = Aeson.object [(Key.fromText (Text.pack "foo"), v)]
    validErrors = TestUtil.validatePure schema (mkInst (Aeson.String (Text.pack "hi")))
    invalidErrors = TestUtil.validatePure schema (mkInst (Aeson.Number 123))
  HUnit.assertBool
    ("spec-valid {foo:\"hi\"} must validate; errors=" <> show validErrors)
    (null validErrors)
  assertSingleError invalidErrors $ \err ->
    case Validate.errorKind err of
      Validate.TypeMismatch{} -> pure ()
      other ->
        HUnit.assertFailure ("expected TypeMismatch for {foo:123}, got: " <> show other)

testResolveUriRfc3986 :: HUnit.Assertion
testResolveUriRfc3986 =
  let
    base = Just (Text.pack "http://a/b/c/d;p?q")
    cases =
      [ ("g:h", "g:h")
      , ("g", "http://a/b/c/g")
      , ("./g", "http://a/b/c/g")
      , ("g/", "http://a/b/c/g/")
      , ("/g", "http://a/g")
      , ("//g", "http://g")
      , ("?y", "http://a/b/c/d;p?y")
      , ("g?y", "http://a/b/c/g?y")
      , ("#s", "http://a/b/c/d;p?q#s")
      , ("g#s", "http://a/b/c/g#s")
      , ("g?y#s", "http://a/b/c/g?y#s")
      , (";x", "http://a/b/c/;x")
      , ("g;x", "http://a/b/c/g;x")
      , ("g;x?y#s", "http://a/b/c/g;x?y#s")
      , ("", "http://a/b/c/d;p?q")
      , (".", "http://a/b/c/")
      , ("./", "http://a/b/c/")
      , ("..", "http://a/b/")
      , ("../", "http://a/b/")
      , ("../g", "http://a/b/g")
      , ("../..", "http://a/")
      , ("../../", "http://a/")
      , ("../../g", "http://a/g")
      , ("../../../g", "http://a/g")
      , ("../../../../g", "http://a/g")
      , ("/./g", "http://a/g")
      , ("/../g", "http://a/g")
      , ("g.", "http://a/b/c/g.")
      , (".g", "http://a/b/c/.g")
      , ("g..", "http://a/b/c/g..")
      , ("..g", "http://a/b/c/..g")
      , ("./../g", "http://a/b/g")
      , ("./g/.", "http://a/b/c/g/")
      , ("g/./h", "http://a/b/c/g/h")
      , ("g/../h", "http://a/b/c/h")
      , ("g;x=1/./y", "http://a/b/c/g;x=1/y")
      , ("g;x=1/../y", "http://a/b/c/y")
      , ("g?y/./x", "http://a/b/c/g?y/./x")
      , ("g?y/../x", "http://a/b/c/g?y/../x")
      , ("g#s/./x", "http://a/b/c/g#s/./x")
      , ("g#s/../x", "http://a/b/c/g#s/../x")
      , ("http:g", "http:g")
      ]
   in
    do
      Foldable.for_ cases $ \(reference, expected) ->
        HUnit.assertEqual reference (Text.pack expected) (Uri.resolveUri base (Text.pack reference))
      HUnit.assertEqual
        "query with slash in base"
        (Text.pack "http://a/b/c/g")
        (Uri.resolveUri (Just (Text.pack "http://a/b/c/d?q=x/y")) (Text.pack "g"))

propPercentDecodeRoundtrip :: H.Property
propPercentDecodeRoundtrip = H.property $ do
  segment <- H.forAll genRefSegment
  Prepass.percentDecode (percentEncodeForTest segment) H.=== segment

genRefSegment :: H.Gen Text.Text
genRefSegment =
  Gen.text (Range.linear 0 12)
    $ Gen.frequency
      [ (3, Gen.element ['~', '/', '%', '"'])
      , (7, Gen.filter (\c -> let n = Char.ord c in n < 0xD800 || n > 0xDFFF) Gen.unicode)
      ]

percentEncodeForTest :: Text.Text -> Text.Text
percentEncodeForTest = Text.concatMap (Text.pack . foldMap hexByte . utf8Bytes)
  where
    utf8Bytes c =
      let
        n = Char.ord c
       in
        if n < 0x80
          then [n]
          else
            if n < 0x800
              then [192 + n `div` 64, 128 + n `mod` 64]
              else
                if n < 0x10000
                  then [224 + n `div` 4096, 128 + (n `div` 64) `mod` 64, 128 + n `mod` 64]
                  else
                    [ 240 + n `div` 262144
                    , 128 + (n `div` 4096) `mod` 64
                    , 128 + (n `div` 64) `mod` 64
                    , 128 + n `mod` 64
                    ]
    hexByte b = ['%', Char.intToDigit (b `div` 16), Char.intToDigit (b `mod` 16)]

testPercentDecodePins :: HUnit.Assertion
testPercentDecodePins = do
  HUnit.assertEqual "%22 decodes to a quote" (Text.pack "\"") (Prepass.percentDecode (Text.pack "%22"))
  HUnit.assertEqual "%25 decodes to a percent" (Text.pack "%") (Prepass.percentDecode (Text.pack "%25"))
  HUnit.assertEqual
    "%7E0 decodes to ~0 (tilde-unescape sees it later)"
    (Text.pack "~0")
    (Prepass.percentDecode (Text.pack "%7E0"))
  HUnit.assertEqual "malformed %zz left literal" (Text.pack "%zz") (Prepass.percentDecode (Text.pack "%zz"))
  HUnit.assertEqual "invalid UTF-8 lead left literal" (Text.pack "%FF") (Prepass.percentDecode (Text.pack "%FF"))
  HUnit.assertEqual "non-BMP decodes" (Text.pack "\x1F600") (Prepass.percentDecode (Text.pack "%F0%9F%98%80"))

testParseSimpleRefDecoding :: HUnit.Assertion
testParseSimpleRefDecoding = do
  HUnit.assertEqual
    "tilde escape unescaped"
    (Just (Prepass.SimpleRefDef (Text.pack "tilde~field")))
    (Prepass.parseSimpleRef (Text.pack "#/$defs/tilde~0field"))
  HUnit.assertEqual
    "percent-encoded quote decoded"
    (Just (Prepass.SimpleRefDef (Text.pack "foo\"bar")))
    (Prepass.parseSimpleRef (Text.pack "#/$defs/foo%22bar"))
  HUnit.assertEqual
    "percent-encoded anchor character decoded"
    (Just (Prepass.SimpleRefAnchor (Text.pack "my_anchor")))
    (Prepass.parseSimpleRef (Text.pack "#my%5Fanchor"))

testPerResourceDefs :: HUnit.Assertion
testPerResourceDefs = do
  schema <-
    decodeSchemaOrFail
      $ jsonObject
        [ (Text.pack "$id", Aeson.String (Text.pack "http://example.invalid/root.json"))
        ,
          ( Text.pack "properties"
          , jsonObject
              [
                ( Text.pack "foo"
                , jsonObject
                    [ (Text.pack "$id", Aeson.String (Text.pack "inner.json"))
                    ,
                      ( Text.pack "$defs"
                      , jsonObject
                          [
                            ( Text.pack "inner"
                            , jsonObject
                                [
                                  ( Text.pack "properties"
                                  , jsonObject [(Text.pack "bar", jsonObject [(Text.pack "type", Aeson.String (Text.pack "string"))])]
                                  )
                                ]
                            )
                          ]
                      )
                    , (Text.pack "$ref", Aeson.String (Text.pack "#/$defs/inner"))
                    ]
                )
              ]
          )
        , (Text.pack "$ref", Aeson.String (Text.pack "inner.json"))
        ]
  let
    validInst =
      jsonObject
        [ (Text.pack "foo", jsonObject [(Text.pack "bar", Aeson.String (Text.pack "a"))])
        , (Text.pack "bar", Aeson.String (Text.pack "a"))
        ]
    invalidInner =
      jsonObject
        [(Text.pack "foo", jsonObject [(Text.pack "bar", Aeson.Number 1)]), (Text.pack "bar", Aeson.String (Text.pack "a"))]
    validErrors = TestUtil.validatePure schema validInst
    invalidErrors = TestUtil.validatePure schema invalidInner
  HUnit.assertBool ("expected valid instance to validate; errors=" <> show validErrors) (null validErrors)
  expectNoRefUnresolved invalidErrors
  assertSingleError invalidErrors $ \err ->
    case Validate.errorKind err of
      Validate.TypeMismatch{} -> pure ()
      other -> HUnit.assertFailure ("expected TypeMismatch on the inner field, got: " <> show other)

expectNoRefUnresolved :: [Validate.ValidationError] -> HUnit.Assertion
expectNoRefUnresolved errors =
  HUnit.assertBool
    ("expected no RefUnresolved faults, got: " <> show errors)
    (not (any (TestUtil.isRefUnresolved . Validate.errorKind) errors))

testUriRefAnchorFragment :: HUnit.Assertion
testUriRefAnchorFragment = do
  schema <-
    decodeSchemaOrFail
      $ jsonObject
        [ (Text.pack "$id", Aeson.String (Text.pack "urn:uuid:deadbeef-1234-ff00-00ff-4321feebdaed"))
        ,
          ( Text.pack "properties"
          , jsonObject
              [
                ( Text.pack "foo"
                , jsonObject [(Text.pack "$ref", Aeson.String (Text.pack "urn:uuid:deadbeef-1234-ff00-00ff-4321feebdaed#something"))]
                )
              ]
          )
        ,
          ( Text.pack "$defs"
          , jsonObject
              [
                ( Text.pack "bar"
                , jsonObject
                    [ (Text.pack "$anchor", Aeson.String (Text.pack "something"))
                    , (Text.pack "type", Aeson.String (Text.pack "string"))
                    ]
                )
              ]
          )
        ]
  let
    validErrors = TestUtil.validatePure schema (jsonObject [(Text.pack "foo", Aeson.String (Text.pack "bar"))])
    invalidErrors = TestUtil.validatePure schema (jsonObject [(Text.pack "foo", Aeson.Number 12)])
  HUnit.assertBool ("expected {foo:\"bar\"} to validate; errors=" <> show validErrors) (null validErrors)
  expectNoRefUnresolved invalidErrors
  assertSingleError invalidErrors $ \err ->
    case Validate.errorKind err of
      Validate.TypeMismatch{} -> pure ()
      other -> HUnit.assertFailure ("expected TypeMismatch under the anchor's type, got: " <> show other)

testSameAnchorDifferentBase :: HUnit.Assertion
testSameAnchorDifferentBase = do
  schema <-
    decodeSchemaOrFail
      $ jsonObject
        [ (Text.pack "$id", Aeson.String (Text.pack "http://example.invalid/foobar"))
        ,
          ( Text.pack "$defs"
          , jsonObject
              [
                ( Text.pack "A"
                , jsonObject
                    [ (Text.pack "$id", Aeson.String (Text.pack "child1"))
                    ,
                      ( Text.pack "allOf"
                      , Aeson.Array
                          $ Vector.fromList
                            [ jsonObject
                                [ (Text.pack "$id", Aeson.String (Text.pack "child2"))
                                , (Text.pack "$anchor", Aeson.String (Text.pack "my_anchor"))
                                , (Text.pack "type", Aeson.String (Text.pack "number"))
                                ]
                            , jsonObject
                                [ (Text.pack "$anchor", Aeson.String (Text.pack "my_anchor"))
                                , (Text.pack "type", Aeson.String (Text.pack "string"))
                                ]
                            ]
                      )
                    ]
                )
              ]
          )
        , (Text.pack "$ref", Aeson.String (Text.pack "child1#my_anchor"))
        ]
  let
    validErrors = TestUtil.validatePure schema (Aeson.String (Text.pack "a"))
    invalidErrors = TestUtil.validatePure schema (Aeson.Number 1)
  HUnit.assertBool ("expected \"a\" to validate via the child1 anchor; errors=" <> show validErrors) (null validErrors)
  expectNoRefUnresolved invalidErrors
  assertSingleError invalidErrors $ \err ->
    case Validate.errorKind err of
      Validate.TypeMismatch{} -> pure ()
      other -> HUnit.assertFailure ("expected TypeMismatch (resolved to child1's string anchor), got: " <> show other)

testForeignContextDefs :: HUnit.Assertion
testForeignContextDefs = do
  mainSchema <-
    decodeSchemaOrFail
      $ jsonObject
        [ (Text.pack "$id", Aeson.String (Text.pack "http://example.invalid/main.json"))
        , (Text.pack "$ref", Aeson.String (Text.pack "sub.json#/$defs/refToInteger"))
        ]
  subSchema <-
    decodeSchemaOrFail
      $ jsonObject
        [
          ( Text.pack "$defs"
          , jsonObject
              [ (Text.pack "integer", jsonObject [(Text.pack "type", Aeson.String (Text.pack "integer"))])
              , (Text.pack "refToInteger", jsonObject [(Text.pack "$ref", Aeson.String (Text.pack "#/$defs/integer"))])
              ]
          )
        ]
  let
    subUri = Text.pack "http://example.invalid/sub.json"
    registry =
      Validate.insertResource subUri subSchema Validate.emptyRegistry
    validErrors = Validate.validate (Validate.compile registry mainSchema) (Aeson.Number 1)
    invalidErrors = Validate.validate (Validate.compile registry mainSchema) (Aeson.String (Text.pack "a"))
  HUnit.assertBool ("expected 1 to validate via the foreign $defs; errors=" <> show validErrors) (null validErrors)
  expectNoRefUnresolved invalidErrors
  assertSingleError invalidErrors $ \err ->
    case Validate.errorKind err of
      Validate.TypeMismatch{} -> pure ()
      other -> HUnit.assertFailure ("expected TypeMismatch for \"a\", got: " <> show other)

testForeignHashRewriteWhiteBox :: HUnit.Assertion
testForeignHashRewriteWhiteBox = do
  mainSchema <-
    decodeSchemaOrFail
      $ jsonObject
        [ (Text.pack "$id", Aeson.String (Text.pack "http://example.invalid/object"))
        ,
          ( Text.pack "properties"
          , jsonObject
              [
                ( Text.pack "name"
                , jsonObject [(Text.pack "$ref", Aeson.String (Text.pack "name-defs.json#/$defs/orNull"))]
                )
              ]
          )
        ]
  nameDefsSchema <- decodeSchemaOrFail nameDefsValue
  let
    resolved =
      Prepass.resolvedToSchema
        ( Prepass.resolveResource
            (nameDefsRegistry nameDefsSchema)
            (Prepass.schemaBaseUri mainSchema)
            (Registry.resource mainSchema)
        )
  case resolved of
    Schema.ObjectSchema obj
      | Just props <- Schema.schemaProperties obj
      , Just (Schema.ObjectSchema nameObj) <- OMap.lookup (Text.pack "name") props
      , Just anyOfBranches <- Schema.schemaAnyOf nameObj
      , [_nullBranch, Schema.ObjectSchema refBranch] <- NEL.toList anyOfBranches -> do
          HUnit.assertEqual
            "foreign # inlined as the foreign root (type string)"
            (Just (NEL.singleton Schema.TypeString))
            (Schema.schemaType refBranch)
          HUnit.assertEqual
            "no literal # ref remains for the main-root knot to misread"
            Nothing
            (Schema.schemaRef refBranch)
    _ -> HUnit.assertFailure ("unexpected resolved shape: " <> show resolved)

nameDefsValue :: Aeson.Value
nameDefsValue =
  jsonObject
    [
      ( Text.pack "$defs"
      , jsonObject
          [
            ( Text.pack "orNull"
            , jsonObject
                [
                  ( Text.pack "anyOf"
                  , Aeson.Array
                      $ Vector.fromList
                        [ jsonObject [(Text.pack "type", Aeson.String (Text.pack "null"))]
                        , jsonObject [(Text.pack "$ref", Aeson.String (Text.pack "#"))]
                        ]
                  )
                ]
            )
          ]
      )
    , (Text.pack "type", Aeson.String (Text.pack "string"))
    ]

nameDefsRegistry :: Schema.Schema -> Validate.Registry
nameDefsRegistry nameDefsSchema =
  Validate.insertResource nameDefsUri nameDefsSchema Validate.emptyRegistry

nameDefsUri :: Text.Text
nameDefsUri = Text.pack "http://example.invalid/name-defs.json"

testForeignHashRewriteBlackBox :: HUnit.Assertion
testForeignHashRewriteBlackBox = do
  mainSchema <-
    decodeSchemaOrFail
      $ jsonObject
        [ (Text.pack "$id", Aeson.String (Text.pack "http://example.invalid/object"))
        , (Text.pack "type", Aeson.String (Text.pack "object"))
        ,
          ( Text.pack "properties"
          , jsonObject
              [
                ( Text.pack "name"
                , jsonObject [(Text.pack "$ref", Aeson.String (Text.pack "name-defs.json#/$defs/orNull"))]
                )
              ]
          )
        ]
  nameDefsSchema <- decodeSchemaOrFail nameDefsValue
  let
    registry = nameDefsRegistry nameDefsSchema
    nameIs v = jsonObject [(Text.pack "name", v)]
    stringErrors = Validate.validate (Validate.compile registry mainSchema) (nameIs (Aeson.String (Text.pack "foo")))
    numberErrors = Validate.validate (Validate.compile registry mainSchema) (nameIs (Aeson.Number 3))
    nestedErrors = Validate.validate (Validate.compile registry mainSchema) (nameIs (jsonObject [(Text.pack "name", Aeson.Null)]))
  HUnit.assertBool ("expected {name:\"foo\"} to validate; errors=" <> show stringErrors) (null stringErrors)
  expectNoRefUnresolved numberErrors
  HUnit.assertBool "expected {name:3} to fail under the foreign string type" (not (null numberErrors))
  HUnit.assertBool
    "expected {name:{name:null}} to fail — the foreign # means the foreign root, not the main root"
    (not (null nestedErrors))

testLateBoundTargetKnot :: HUnit.Assertion
testLateBoundTargetKnot = do
  mainSchema <-
    decodeSchemaOrFail
      $ jsonObject
        [ (Text.pack "$ref", Aeson.String lateBoundBUri)
        ,
          ( Text.pack "not"
          , jsonObject
              [
                ( Text.pack "required"
                , Aeson.Array (Vector.singleton (Aeson.String (Text.pack "n")))
                )
              ]
          )
        ]
  bSchema <- decodeSchemaOrFail lateBoundBValue
  let
    errors =
      Validate.validate
        (Validate.compile (Validate.addSchema bSchema Validate.emptyRegistry) mainSchema)
        (jsonObject [(Text.pack "child", jsonObject [(Text.pack "n", Aeson.String (Text.pack "x"))])])
  HUnit.assertBool
    ("expected the late-bound target to impose nothing on the child; errors=" <> show errors)
    (null errors)

lateBoundBUri :: Text.Text
lateBoundBUri = Text.pack "http://example.invalid/b"

lateBoundBValue :: Aeson.Value
lateBoundBValue =
  jsonObject
    [ (Text.pack "$id", Aeson.String lateBoundBUri)
    ,
      ( Text.pack "properties"
      , jsonObject
          [
            ( Text.pack "child"
            , jsonObject [(Text.pack "$ref", Aeson.String (Text.pack "#"))]
            )
          ]
      )
    ]

testCachedTargetKnot :: HUnit.Assertion
testCachedTargetKnot = do
  mainSchema <-
    decodeSchemaOrFail
      $ jsonObject
        [ (Text.pack "$dynamicRef", Aeson.String lateBoundBUri)
        ,
          ( Text.pack "not"
          , jsonObject
              [
                ( Text.pack "required"
                , Aeson.Array (Vector.singleton (Aeson.String (Text.pack "n")))
                )
              ]
          )
        ]
  bSchema <- decodeSchemaOrFail lateBoundBValue
  let
    errors =
      Validate.validate
        (Validate.compile (Validate.addSchema bSchema Validate.emptyRegistry) mainSchema)
        (jsonObject [(Text.pack "child", jsonObject [(Text.pack "n", Aeson.String (Text.pack "x"))])])
  HUnit.assertBool
    ("expected the cached target to impose nothing on the child; errors=" <> show errors)
    (null errors)

testEmbeddedMetaschemasDecode :: HUnit.Assertion
testEmbeddedMetaschemasDecode = do
  HUnit.assertEqual "embedded document count" 9 (length Metaschema.metaschemaDocuments)
  Foldable.for_ Metaschema.metaschemaDocuments $ \(path, decoded) ->
    case decoded of
      Left err -> HUnit.assertFailure (path <> ": " <> err)
      Right _ -> pure ()

testMetaschemaVocabularies :: HUnit.Assertion
testMetaschemaVocabularies =
  Dialect.dialectVocabularies
    (Dialect.dialectForMetaschema Metaschema.metaschemaRegistry (Text.pack "https://json-schema.org/draft/2020-12/schema"))
    HUnit.@?= Dialect.standardVocabularies

testReadMaybeIntLeadingZeros :: HUnit.Assertion
testReadMaybeIntLeadingZeros =
  fmap Prepass.readMaybeInt [Text.pack "0", Text.pack "10", Text.pack "01", Text.pack "00"]
    HUnit.@?= [Just 0, Just 10, Nothing, Nothing]

testDefsSelfCycle :: HUnit.Assertion
testDefsSelfCycle =
  assertRefCycle
    $ jsonObject
      [ (Text.pack "$defs", jsonObject [(Text.pack "a", refTo "#/$defs/a")])
      , (Text.pack "$ref", Aeson.String (Text.pack "#/$defs/a"))
      ]

assertRefCycle :: Aeson.Value -> HUnit.Assertion
assertRefCycle schemaValue = do
  errors <- validateWithinTimeout schemaValue Aeson.Null
  HUnit.assertBool
    ("expected a RefCycle error, got: " <> show errors)
    (any (TestUtil.isRefCycle . Validate.errorKind) errors)

validateWithinTimeout :: Aeson.Value -> Aeson.Value -> IO [Validate.ValidationError]
validateWithinTimeout schemaValue instanceValue = do
  schema <- decodeSchemaOrFail schemaValue
  TestUtil.withinTimeout (Validate.validateOn schema instanceValue)

testDefsMutualCycle :: HUnit.Assertion
testDefsMutualCycle =
  assertRefCycle
    $ jsonObject
      [ (Text.pack "$defs", jsonObject [(Text.pack "a", refTo "#/$defs/b"), (Text.pack "b", refTo "#/$defs/a")])
      , (Text.pack "$ref", Aeson.String (Text.pack "#/$defs/a"))
      ]

testAllOfRootCycle :: HUnit.Assertion
testAllOfRootCycle =
  assertRefCycle $ jsonObject [(Text.pack "allOf", Aeson.Array (Vector.singleton (refTo "#")))]

testUriSelfCycle :: HUnit.Assertion
testUriSelfCycle =
  assertRefCycle
    $ jsonObject
      [ (Text.pack "$id", Aeson.String (Text.pack "http://example.invalid/self"))
      , (Text.pack "$ref", Aeson.String (Text.pack "http://example.invalid/self"))
      ]

testAnchorSelfCycle :: HUnit.Assertion
testAnchorSelfCycle =
  assertRefCycle
    $ jsonObject
      [
        ( Text.pack "$defs"
        , jsonObject
            [
              ( Text.pack "a"
              , jsonObject [(Text.pack "$anchor", Aeson.String (Text.pack "a")), (Text.pack "$ref", Aeson.String (Text.pack "#a"))]
              )
            ]
        )
      , (Text.pack "$ref", Aeson.String (Text.pack "#a"))
      ]

testRecursiveTreeValidates :: HUnit.Assertion
testRecursiveTreeValidates = do
  let
    leaf = jsonObject [(Text.pack "children", Aeson.Array Vector.empty)]
    nestedTree =
      jsonObject
        [
          ( Text.pack "children"
          , Aeson.Array (Vector.fromList [leaf, jsonObject [(Text.pack "children", Aeson.Array (Vector.singleton leaf))]])
          )
        ]
    badTree = jsonObject [(Text.pack "children", Aeson.Array (Vector.singleton (Aeson.Number 1)))]
  validErrors <- validateWithinTimeout recursiveTreeSchema nestedTree
  invalidErrors <- validateWithinTimeout recursiveTreeSchema badTree
  validErrors HUnit.@?= []
  HUnit.assertBool "expected a nested type error" (not (null invalidErrors))

recursiveTreeSchema :: Aeson.Value
recursiveTreeSchema =
  let
    nodeSchema =
      jsonObject
        [ (Text.pack "type", Aeson.String (Text.pack "object"))
        ,
          ( Text.pack "properties"
          , jsonObject
              [
                ( Text.pack "children"
                , jsonObject [(Text.pack "type", Aeson.String (Text.pack "array")), (Text.pack "items", refTo "#/$defs/node")]
                )
              ]
          )
        ]
   in
    jsonObject
      [ (Text.pack "$defs", jsonObject [(Text.pack "node", nodeSchema)])
      , (Text.pack "$ref", Aeson.String (Text.pack "#/$defs/node"))
      ]

testCheckInvalidPattern :: HUnit.Assertion
testCheckInvalidPattern =
  assertCheck
    (jsonObject [(Text.pack "pattern", Aeson.String (Text.pack "("))])
    (Left (Validate.InvalidPatternError (Text.pack "pattern") (Text.pack "(")))

assertCheck :: Aeson.Value -> Either Validate.SchemaError () -> HUnit.Assertion
assertCheck schemaValue expected = do
  result <- checkWithinTimeout Validate.emptyRegistry schemaValue
  result HUnit.@?= expected

checkWithinTimeout :: Validate.Registry -> Aeson.Value -> IO (Either Validate.SchemaError ())
checkWithinTimeout registry schemaValue = do
  schema <- decodeSchemaOrFail schemaValue
  TestUtil.withinTimeout (Validate.checkSchema registry schema)

testCheckMissingDef :: HUnit.Assertion
testCheckMissingDef =
  assertCheck
    (jsonObject [(Text.pack "properties", jsonObject [(Text.pack "a", refTo "#/$defs/x")])])
    (Left (Validate.UnresolvableReferenceError (Text.pack "#/$defs/x")))

testCheckMissingAnchor :: HUnit.Assertion
testCheckMissingAnchor =
  assertCheck
    (jsonObject [(Text.pack "items", refTo "#node")])
    (Left (Validate.UnresolvableReferenceError (Text.pack "#node")))

testCheckRegisteredAnchor :: HUnit.Assertion
testCheckRegisteredAnchor =
  assertCheck
    ( jsonObject
        [ (Text.pack "$defs", jsonObject [(Text.pack "n", jsonObject [(Text.pack "$anchor", Aeson.String (Text.pack "node"))])])
        , (Text.pack "items", refTo "#node")
        ]
    )
    (Right ())

testCheckPlainDynamicRef :: HUnit.Assertion
testCheckPlainDynamicRef =
  assertCheck (itemsDynamicRefTo "#node") (Right ())

itemsDynamicRefTo :: String -> Aeson.Value
itemsDynamicRefTo target =
  jsonObject [nodeDefs (jsonObject []), (Text.pack "items", dynamicRefTo target)]

testCheckDynamicRefUnknownResource :: HUnit.Assertion
testCheckDynamicRefUnknownResource =
  assertCheck
    (itemsDynamicRefTo "http://x/other.json#node")
    (Left (Validate.UnresolvableReferenceError (Text.pack "http://x/other.json#node")))

testCheckRootRef :: HUnit.Assertion
testCheckRootRef =
  assertCheck (jsonObject [(Text.pack "items", refTo "#")]) (Right ())

testCheckCycle :: HUnit.Assertion
testCheckCycle =
  assertCheck
    ( jsonObject
        [ (Text.pack "$defs", jsonObject [(Text.pack "a", refTo "#/$defs/b"), (Text.pack "b", refTo "#/$defs/a")])
        , (Text.pack "$ref", Aeson.String (Text.pack "#/$defs/a"))
        ]
    )
    (Left (Validate.ReferenceCycleError (Text.pack "#/$defs/b")))

testCheckRecursiveTree :: HUnit.Assertion
testCheckRecursiveTree =
  assertCheck recursiveTreeSchema (Right ())

testDecodeRejectsBadAnchor :: HUnit.Assertion
testDecodeRejectsBadAnchor =
  assertDecodeRejects (jsonObject [(Text.pack "$anchor", Aeson.String (Text.pack "1node"))])

assertDecodeRejects :: Aeson.Value -> HUnit.Assertion
assertDecodeRejects value =
  HUnit.assertBool ("expected decode failure for " <> show value)
    . Either.isLeft
    $ TestUtil.decodeSchemaValue value

testDecodeRejectsBadDynamicAnchor :: HUnit.Assertion
testDecodeRejectsBadDynamicAnchor =
  assertDecodeRejects (jsonObject [(Text.pack "$dynamicAnchor", Aeson.String (Text.pack "a/b"))])

testDecodeRejectsIdFragment :: HUnit.Assertion
testDecodeRejectsIdFragment =
  assertDecodeRejects (jsonObject [(Text.pack "$id", Aeson.String (Text.pack "https://example.invalid/s#frag"))])

testDecodeAcceptsEmptyIdFragment :: HUnit.Assertion
testDecodeAcceptsEmptyIdFragment = do
  schema <- decodeSchemaOrFail (jsonObject [(Text.pack "$id", Aeson.String (Text.pack "https://example.invalid/s#"))])
  HUnit.assertBool "expected an object schema" $ case schema of
    Schema.ObjectSchema _ -> True
    Schema.BoolSchema _ -> False

testDereference :: HUnit.Assertion
testDereference = do
  let
    nameSchema = jsonObject [(Text.pack "type", Aeson.String (Text.pack "string"))]
    defs = (Text.pack "$defs", jsonObject [(Text.pack "Name", nameSchema)])
    missing = refTo "#/$defs/Missing"
  schema <-
    decodeSchemaOrFail
      $ jsonObject
        [defs, (Text.pack "properties", jsonObject [(Text.pack "a", refTo "#/$defs/Name"), (Text.pack "b", missing)])]
  Aeson.toJSON (Validate.dereference Validate.emptyRegistry schema)
    HUnit.@?= jsonObject
      [defs, (Text.pack "properties", jsonObject [(Text.pack "a", nameSchema), (Text.pack "b", missing)])]

testCustomFormat :: HUnit.Assertion
testCustomFormat =
  let
    registry = Validate.withFormat (Validate.Format (Text.pack "hex")) hexPredicate Validate.emptyRegistry
    schema =
      Schema.ObjectSchema
        Schema.emptySchema{Schema.schemaFormat = Just (Text.pack "hex")}
   in
    do
      HUnit.assertBool "valid hex accepted" . null
        $ Validate.validate (Validate.compile registry schema) (Aeson.String (Text.pack "deadbeef"))
      HUnit.assertBool "valid hex uppercase accepted" . null
        $ Validate.validate (Validate.compile registry schema) (Aeson.String (Text.pack "CAFE"))

hexPredicate :: Validate.FormatPredicate
hexPredicate = Validate.FormatPredicate (\t -> not (Text.null t) && Text.all Char.isHexDigit t)

testCustomFormatRejects :: HUnit.Assertion
testCustomFormatRejects =
  let
    registry = Validate.withFormat (Validate.Format (Text.pack "hex")) hexPredicate Validate.emptyRegistry
    schema =
      Schema.ObjectSchema
        Schema.emptySchema{Schema.schemaFormat = Just (Text.pack "hex")}
   in
    HUnit.assertBool "invalid hex rejected" . not . null
      $ Validate.validate (Validate.compile registry schema) (Aeson.String (Text.pack "xyz"))

testOverrideBuiltinFormat :: HUnit.Assertion
testOverrideBuiltinFormat =
  let
    restrictedDate = Validate.FormatPredicate (\t -> Text.isPrefixOf (Text.pack "2024-") t && Text.length t == 10)
    registry = Validate.withFormat (Validate.Format (Text.pack "date")) restrictedDate Validate.emptyRegistry
    schema =
      Schema.ObjectSchema
        Schema.emptySchema{Schema.schemaFormat = Just (Text.pack "date")}
   in
    do
      HUnit.assertBool "2024 date accepted" . null
        $ Validate.validate (Validate.compile registry schema) (Aeson.String (Text.pack "2024-01-15"))
      HUnit.assertBool "2023 date rejected by override" . not . null
        $ Validate.validate (Validate.compile registry schema) (Aeson.String (Text.pack "2023-06-01"))
      HUnit.assertBool "2023 date accepted by default" . null
        $ TestUtil.validatePure schema (Aeson.String (Text.pack "2023-06-01"))

propUriAcceptsScheme :: H.Property
propUriAcceptsScheme = H.property $ do
  t <- H.forAll genNonEmptyText
  H.assert (builtinAccepts (Text.pack "uri") (Text.pack "x:" <> t))

genNonEmptyText :: H.Gen Text.Text
genNonEmptyText = fmap Text.pack . Gen.list (Range.linear 1 32) . Gen.filter (/= '\n') $ Gen.unicode

builtinAccepts :: Text.Text -> Text.Text -> Bool
builtinAccepts fmt input =
  all (`Format.runFormatPredicate` input) (Format.assertedFormat Map.empty True fmt)

testUriRejectsRelative :: HUnit.Assertion
testUriRejectsRelative = do
  formatRejects (Text.pack "uri") Text.empty
  formatRejects (Text.pack "uri") (Text.pack "abc")
  formatRejects (Text.pack "uri") (Text.pack "/abc")
  formatRejects (Text.pack "uri") (Text.pack "//foo.bar/?baz=qux#quux")
  formatRejects (Text.pack "uri") (Text.pack "1http://example.com")

formatRejects :: Text.Text -> Text.Text -> HUnit.Assertion
formatRejects fmt input =
  HUnit.assertBool (Text.unpack fmt <> " should reject " <> Text.unpack input)
    . not
    $ builtinAccepts fmt input

propUriReferenceRelaxed :: H.Property
propUriReferenceRelaxed = H.property $ do
  t <- H.forAll genNonEmptyText
  H.assert (builtinAccepts (Text.pack "uri-reference") t)

testUriReferenceEmpty :: HUnit.Assertion
testUriReferenceEmpty = formatRejects (Text.pack "uri-reference") Text.empty

testIriNotBuiltin :: HUnit.Assertion
testIriNotBuiltin =
  formatAccepts (Text.pack "iri") (Text.pack "not-a-real-iri")

formatAccepts :: Text.Text -> Text.Text -> HUnit.Assertion
formatAccepts fmt input =
  HUnit.assertBool (Text.unpack fmt <> " should accept " <> Text.unpack input)
    $ builtinAccepts fmt input

testIriReferenceNotBuiltin :: HUnit.Assertion
testIriReferenceNotBuiltin =
  formatAccepts (Text.pack "iri-reference") (Text.pack "not-a-real-iri-ref")

testHostnameAscii :: HUnit.Assertion
testHostnameAscii = formatAccepts (Text.pack "hostname") (Text.pack "example.com")

testHostnamePunycode :: HUnit.Assertion
testHostnamePunycode =
  formatAccepts (Text.pack "hostname") (Text.pack "xn--example.com")

testHostnameLeadingHyphen :: HUnit.Assertion
testHostnameLeadingHyphen = formatRejects (Text.pack "hostname") (Text.pack "-bad.com")

testHostnameUnicodeRejected :: HUnit.Assertion
testHostnameUnicodeRejected = formatRejects (Text.pack "hostname") (Text.pack "ünicode.com")

testIdnHostnameAscii :: HUnit.Assertion
testIdnHostnameAscii = formatAccepts (Text.pack "idn-hostname") (Text.pack "example.com")

testIdnHostnameUnicodeRejected :: HUnit.Assertion
testIdnHostnameUnicodeRejected =
  formatRejects (Text.pack "idn-hostname") (Text.pack "ünicode.com")

testIdnEmailAscii :: HUnit.Assertion
testIdnEmailAscii = formatAccepts (Text.pack "idn-email") (Text.pack "user@example.com")

testIdnEmailUnicodeRejected :: HUnit.Assertion
testIdnEmailUnicodeRejected =
  formatRejects (Text.pack "idn-email") (Text.pack "üser@example.com")

testDateOk :: HUnit.Assertion
testDateOk = formatAccepts (Text.pack "date") (Text.pack "2026-05-16")

testDateBad :: HUnit.Assertion
testDateBad = formatRejects (Text.pack "date") (Text.pack "not-a-date")

testDateYearDigits :: HUnit.Assertion
testDateYearDigits = do
  formatRejects (Text.pack "date") (Text.pack "+2020-01-01")
  formatRejects (Text.pack "date") (Text.pack "12345-01-01")
  formatRejects (Text.pack "date") (Text.pack "999-01-01")

testTimeOk :: HUnit.Assertion
testTimeOk = formatAccepts (Text.pack "time") (Text.pack "13:14:15Z")

testTimeBad :: HUnit.Assertion
testTimeBad = formatRejects (Text.pack "time") (Text.pack "25:00:00Z")

testTimeLeapSecondRanges :: HUnit.Assertion
testTimeLeapSecondRanges = do
  formatAccepts (Text.pack "time") (Text.pack "23:59:60Z")
  formatAccepts (Text.pack "time") (Text.pack "00:59:60+01:00")
  formatRejects (Text.pack "time") (Text.pack "25:00:60+01:01")
  formatRejects (Text.pack "time") (Text.pack "23:99:60+00:40")
  formatRejects (Text.pack "time") (Text.pack "23:59:61Z")

testDateTimeOk :: HUnit.Assertion
testDateTimeOk = formatAccepts (Text.pack "date-time") (Text.pack "2026-05-16T13:14:15Z")

testDateTimeBad :: HUnit.Assertion
testDateTimeBad = formatRejects (Text.pack "date-time") (Text.pack "2026-05-16 13:14:15")

testDurationOk :: HUnit.Assertion
testDurationOk = formatAccepts (Text.pack "duration") (Text.pack "P1Y2M3DT4H5M6S")

testDurationBad :: HUnit.Assertion
testDurationBad = formatRejects (Text.pack "duration") (Text.pack "P")

testEmailOk :: HUnit.Assertion
testEmailOk = formatAccepts (Text.pack "email") (Text.pack "user@example.com")

testEmailBad :: HUnit.Assertion
testEmailBad = formatRejects (Text.pack "email") (Text.pack "no-at-sign")

testIpv4Ok :: HUnit.Assertion
testIpv4Ok = formatAccepts (Text.pack "ipv4") (Text.pack "192.168.0.1")

testIpv4Bad :: HUnit.Assertion
testIpv4Bad = formatRejects (Text.pack "ipv4") (Text.pack "999.999.999.999")

testIpv6Ok :: HUnit.Assertion
testIpv6Ok = formatAccepts (Text.pack "ipv6") (Text.pack "::1")

testIpv6Bad :: HUnit.Assertion
testIpv6Bad = formatRejects (Text.pack "ipv6") (Text.pack "not-an-ip")

testUriTemplateOk :: HUnit.Assertion
testUriTemplateOk = formatAccepts (Text.pack "uri-template") (Text.pack "/users/{id}")

testUriTemplateBad :: HUnit.Assertion
testUriTemplateBad = formatRejects (Text.pack "uri-template") (Text.pack "/users/{id")

testJsonPointerOk :: HUnit.Assertion
testJsonPointerOk = do
  formatAccepts (Text.pack "json-pointer") Text.empty
  formatAccepts (Text.pack "json-pointer") (Text.pack "/a/b")

testJsonPointerBad :: HUnit.Assertion
testJsonPointerBad = formatRejects (Text.pack "json-pointer") (Text.pack "a/b")

testJsonPointerBadEscape :: HUnit.Assertion
testJsonPointerBadEscape = do
  formatRejects (Text.pack "json-pointer") (Text.pack "/~~10")
  formatRejects (Text.pack "json-pointer") (Text.pack "/~2")
  formatAccepts (Text.pack "json-pointer") (Text.pack "/~1~0~0~1~1")

testRelJsonPointerOk :: HUnit.Assertion
testRelJsonPointerOk = formatAccepts (Text.pack "relative-json-pointer") (Text.pack "2/a/b")

testRelJsonPointerBad :: HUnit.Assertion
testRelJsonPointerBad = formatRejects (Text.pack "relative-json-pointer") (Text.pack "/a/b")

testRegexOk :: HUnit.Assertion
testRegexOk = formatAccepts (Text.pack "regex") (Text.pack "^[a-z]+$")

testRegexBad :: HUnit.Assertion
testRegexBad = formatRejects (Text.pack "regex") (Text.pack "(unclosed")

testUuidOk :: HUnit.Assertion
testUuidOk = formatAccepts (Text.pack "uuid") (Text.pack "f47ac10b-58cc-4372-a567-0e02b2c3d479")

testUuidBad :: HUnit.Assertion
testUuidBad = formatRejects (Text.pack "uuid") (Text.pack "not-a-uuid")

testFormatAnnotationDefault :: HUnit.Assertion
testFormatAnnotationDefault =
  HUnit.assertBool "invalid email validates under default annotation"
    . null
    $ TestUtil.validatePure (formatSchema (Text.pack "email")) invalidEmailValue

formatSchema :: Text.Text -> Schema.Schema
formatSchema fmt =
  Schema.ObjectSchema Schema.emptySchema{Schema.schemaFormat = Just fmt}

invalidEmailValue :: Aeson.Value
invalidEmailValue = Aeson.String (Text.pack "not-an-email")

testFormatCanonicalMetaschemaDefault :: HUnit.Assertion
testFormatCanonicalMetaschemaDefault =
  HUnit.assertBool "unknown metaschema defaults to annotation (Core §8.1.2.1)"
    . null
    $ TestUtil.validatePure (schemaWithMetaschema canonicalMetaschemaUri) invalidEmailValue

schemaWithMetaschema :: Text.Text -> Schema.Schema
schemaWithMetaschema metaUri =
  Schema.ObjectSchema
    Schema.emptySchema
      { Schema.schemaMetaschema = Just (Schema.MetaschemaUri metaUri)
      , Schema.schemaFormat = Just (Text.pack "email")
      }

canonicalMetaschemaUri :: Text.Text
canonicalMetaschemaUri = Text.pack "https://json-schema.org/draft/2020-12/schema"

testFormatVocabularyFalseAsserts :: HUnit.Assertion
testFormatVocabularyFalseAsserts =
  HUnit.assertBool "format-assertion declared false still asserts (Core §8.1.2-4)"
    . not
    . null
    $ Validate.validate
      (Validate.compile (assertionRegistry False) (schemaWithMetaschema vocabularyMetaUri))
      invalidEmailValue

assertionRegistry :: Bool -> Validate.Registry
assertionRegistry required =
  Validate.insertResource vocabularyMetaUri (assertionMetaschema required) Validate.emptyRegistry

vocabularyMetaUri :: Text.Text
vocabularyMetaUri = Text.pack "https://example.com/meta"

assertionMetaschema :: Bool -> Schema.Schema
assertionMetaschema required =
  Schema.ObjectSchema
    Schema.emptySchema
      { Schema.schemaId = Just (Schema.SchemaId vocabularyMetaUri)
      , Schema.schemaVocabulary =
          Just
            ( Map.fromSet (const True) Dialect.standardVocabularies
                <> Map.singleton Dialect.formatAssertionVocabularyUri required
            )
      }

testFormatVocabularyTrueAsserts :: HUnit.Assertion
testFormatVocabularyTrueAsserts =
  HUnit.assertBool "format-assertion declared true asserts"
    . not
    . null
    $ Validate.validate (Validate.compile (assertionRegistry True) (schemaWithMetaschema vocabularyMetaUri)) invalidEmailValue

testFormatVocabularyRecursion :: HUnit.Assertion
testFormatVocabularyRecursion =
  let
    innerUri = Text.pack "https://example.com/meta-inner"
    outerUri = Text.pack "https://example.com/meta-outer"
    inner =
      Schema.ObjectSchema
        Schema.emptySchema
          { Schema.schemaId = Just (Schema.SchemaId innerUri)
          , Schema.schemaVocabulary = Just (Map.singleton Dialect.formatAssertionVocabularyUri True)
          }
    outer =
      Schema.ObjectSchema
        Schema.emptySchema
          { Schema.schemaId = Just (Schema.SchemaId outerUri)
          , Schema.schemaMetaschema = Just (Schema.MetaschemaUri innerUri)
          }
    registry =
      Validate.insertResource innerUri inner . Validate.insertResource outerUri outer $ Validate.emptyRegistry
   in
    HUnit.assertBool "assertion recurses through $vocabulary-less metaschemas"
      . not
      . null
      $ Validate.validate (Validate.compile registry (schemaWithMetaschema outerUri)) invalidEmailValue

testFormatSelfReferentialMetaschema :: HUnit.Assertion
testFormatSelfReferentialMetaschema =
  let
    selfUri = Text.pack "https://example.com/meta-self"
    meta =
      Schema.ObjectSchema
        Schema.emptySchema
          { Schema.schemaId = Just (Schema.SchemaId selfUri)
          , Schema.schemaMetaschema = Just (Schema.MetaschemaUri selfUri)
          }
    registry =
      Validate.insertResource selfUri meta Validate.emptyRegistry
   in
    HUnit.assertBool "self-referential metaschema terminates in annotation"
      . null
      $ Validate.validate (Validate.compile registry (schemaWithMetaschema selfUri)) invalidEmailValue

testFormatDialectInheritance :: HUnit.Assertion
testFormatDialectInheritance =
  let
    schema =
      Schema.ObjectSchema
        Schema.emptySchema
          { Schema.schemaMetaschema = Just (Schema.MetaschemaUri vocabularyMetaUri)
          , Schema.schemaProperties =
              Just (OMap.fromList [(Text.pack "email", formatSchema (Text.pack "email"))])
          }
    instanceValue =
      Aeson.Object (KeyMap.singleton (Key.fromText (Text.pack "email")) invalidEmailValue)
   in
    HUnit.assertBool "root $schema governs nested subschemas"
      . not
      . null
      $ Validate.validate (Validate.compile (assertionRegistry True) schema) instanceValue

testFormatDialectOverride :: HUnit.Assertion
testFormatDialectOverride =
  let
    resource =
      Schema.ObjectSchema
        Schema.emptySchema
          { Schema.schemaId = Just (Schema.SchemaId (Text.pack "https://example.com/res"))
          , Schema.schemaMetaschema = Just (Schema.MetaschemaUri canonicalMetaschemaUri)
          , Schema.schemaFormat = Just (Text.pack "email")
          }
    schema =
      Schema.ObjectSchema
        Schema.emptySchema
          { Schema.schemaMetaschema = Just (Schema.MetaschemaUri vocabularyMetaUri)
          , Schema.schemaProperties = Just (OMap.fromList [(Text.pack "x", resource)])
          }
    instanceValue =
      Aeson.Object (KeyMap.singleton (Key.fromText (Text.pack "x")) invalidEmailValue)
   in
    HUnit.assertBool "nested resource $schema replaces, not merges, the dialect"
      . null
      $ Validate.validate (Validate.compile (assertionRegistry True) schema) instanceValue

testFormatDynamicTargetDialect :: HUnit.Assertion
testFormatDynamicTargetDialect =
  let
    resourceUri = Text.pack "https://example.com/emails"
    target =
      Schema.ObjectSchema
        Schema.emptySchema
          { Schema.schemaDynamicAnchor = Just (Schema.DynamicAnchor (Text.pack "email"))
          , Schema.schemaFormat = Just (Text.pack "email")
          }
    resource =
      Schema.ObjectSchema
        Schema.emptySchema
          { Schema.schemaId = Just (Schema.SchemaId resourceUri)
          , Schema.schemaMetaschema = Just (Schema.MetaschemaUri vocabularyMetaUri)
          , Schema.schemaDefs = Just (OMap.singleton (Text.pack "email", target))
          }
    schema =
      Schema.ObjectSchema
        Schema.emptySchema{Schema.schemaDynamicRef = Just (Schema.DynamicRef (resourceUri <> Text.pack "#email"))}
    registry = Validate.insertResource resourceUri resource (assertionRegistry True)
   in
    HUnit.assertBool "the target resource's $schema governs a $dynamicRef target"
      . not
      . null
      $ Validate.validate (Validate.compile registry schema) invalidEmailValue

testFormatAssertionFlag :: HUnit.Assertion
testFormatAssertionFlag =
  let
    registry =
      Validate.withFormatAssertion Validate.FormatAssertionAlways Validate.emptyRegistry
   in
    HUnit.assertBool "withFormatAssertion opts into assertion (Validation §7.2.1)"
      . not
      . null
      $ Validate.validate (Validate.compile registry (formatSchema (Text.pack "email"))) invalidEmailValue

testFormatNonStringIgnored :: HUnit.Assertion
testFormatNonStringIgnored =
  HUnit.assertBool "formats ignore non-string instances"
    . null
    $ Validate.validate (Validate.compile (assertionRegistry True) (schemaWithMetaschema vocabularyMetaUri)) (Aeson.Number 42)

propFormatAnnotationNeverAsserts :: H.Property
propFormatAnnotationNeverAsserts = H.property $ do
  t <- H.forAll (Gen.text (Range.linear 0 100) Gen.unicode)
  H.assert . null $ TestUtil.validatePure (formatSchema (Text.pack "date")) (Aeson.String t)

testVocabularyMinimumIgnored :: HUnit.Assertion
testVocabularyMinimumIgnored =
  HUnit.assertBool "minimum is inert without the validation vocabulary"
    . null
    $ Validate.validate
      (Validate.compile (gatingRegistry noValidationVocabularies) noValidationSchema)
      (gatingInstance (Text.pack "numberProperty") (Aeson.Number 1))

gatingRegistry :: Set.Set Text.Text -> Validate.Registry
gatingRegistry vocabs =
  Validate.insertResource gatingMetaUri (gatingMetaschema vocabs) Validate.emptyRegistry

gatingMetaUri :: Text.Text
gatingMetaUri = Text.pack "https://example.com/gating-meta"

gatingMetaschema :: Set.Set Text.Text -> Schema.Schema
gatingMetaschema vocabs =
  Schema.ObjectSchema
    Schema.emptySchema
      { Schema.schemaId = Just (Schema.SchemaId gatingMetaUri)
      , Schema.schemaVocabulary = Just (Map.fromSet (const True) vocabs)
      }

noValidationVocabularies :: Set.Set Text.Text
noValidationVocabularies =
  Set.fromList [Dialect.applicatorVocabularyUri, Dialect.coreVocabularyUri]

noValidationSchema :: Schema.Schema
noValidationSchema =
  Schema.ObjectSchema
    Schema.emptySchema
      { Schema.schemaMetaschema = Just (Schema.MetaschemaUri gatingMetaUri)
      , Schema.schemaProperties =
          Just
            ( OMap.fromList
                [ (Text.pack "badProperty", Schema.BoolSchema False)
                , (Text.pack "numberProperty", Schema.ObjectSchema Schema.emptySchema{Schema.schemaMinimum = Just 10})
                ]
            )
      }

gatingInstance :: Text.Text -> Aeson.Value -> Aeson.Value
gatingInstance key val = Aeson.Object (KeyMap.singleton (Key.fromText key) val)

testVocabularyPropertiesLive :: HUnit.Assertion
testVocabularyPropertiesLive =
  HUnit.assertBool "properties stays live with the applicator vocabulary"
    . not
    . null
    $ Validate.validate
      (Validate.compile (gatingRegistry noValidationVocabularies) noValidationSchema)
      (gatingInstance (Text.pack "badProperty") (Aeson.String (Text.pack "x")))

testDefaultDialectEnforcesMinimum :: HUnit.Assertion
testDefaultDialectEnforcesMinimum =
  let
    schema =
      Schema.ObjectSchema
        Schema.emptySchema
          { Schema.schemaProperties =
              Just
                ( OMap.fromList
                    [ (Text.pack "numberProperty", Schema.ObjectSchema Schema.emptySchema{Schema.schemaMinimum = Just 10})
                    ]
                )
          }
   in
    HUnit.assertBool "minimum enforces under the default dialect"
      . not
      . null
      $ TestUtil.validatePure schema (gatingInstance (Text.pack "numberProperty") (Aeson.Number 1))

testVocabularyEmptyFragment :: HUnit.Assertion
testVocabularyEmptyFragment =
  let
    schema = case noValidationSchema of
      Schema.ObjectSchema obj ->
        Schema.ObjectSchema obj{Schema.schemaMetaschema = Just (Schema.MetaschemaUri (gatingMetaUri <> Text.pack "#"))}
      other -> other
   in
    HUnit.assertBool "minimum is inert under a $schema ending in #"
      . null
      $ Validate.validate
        (Validate.compile (gatingRegistry noValidationVocabularies) schema)
        (gatingInstance (Text.pack "numberProperty") (Aeson.Number 1))

testTypeErrorDetails :: HUnit.Assertion
testTypeErrorDetails =
  let
    schema = Schema.ObjectSchema Schema.emptySchema{Schema.schemaType = Just (NEL.singleton Schema.TypeNumber)}
    errors = TestUtil.validatePure schema (Aeson.String (Text.pack "hello"))
   in
    assertSingleError errors $ \err -> do
      HUnit.assertEqual "keyword" (Just (Text.pack "type")) (Validate.errorKeyword err)
      HUnit.assertEqual "schema path" [Text.pack "type"] (Validate.errorSchemaPath err)
      HUnit.assertEqual "instance path" [] (Validate.errorInstancePath err)

testRequiredErrorDetails :: HUnit.Assertion
testRequiredErrorDetails =
  let
    schema =
      Schema.ObjectSchema
        Schema.emptySchema
          { Schema.schemaRequired = Just (OSet.fromList [Text.pack "name", Text.pack "age"])
          }
    errors = TestUtil.validatePure schema (Aeson.Object KeyMap.empty)
    missing =
      foldMap
        ( \e -> case Validate.errorKind e of
            Validate.RequiredMissing k -> Set.singleton k
            _ -> Set.empty
        )
        errors
   in
    do
      HUnit.assertEqual
        ("expected 2 RequiredMissing faults, got: " <> show errors)
        2
        (length errors)
      HUnit.assertEqual
        "missing keys set"
        (Set.fromList [Text.pack "name", Text.pack "age"])
        missing
      HUnit.assertEqual
        ("schema paths, got: " <> show errors)
        [[Text.pack "required"], [Text.pack "required"]]
        (fmap Validate.errorSchemaPath errors)

testNestedPropertyErrorDetails :: HUnit.Assertion
testNestedPropertyErrorDetails =
  let
    schema =
      Schema.ObjectSchema
        Schema.emptySchema
          { Schema.schemaProperties =
              Just
                ( OMap.fromList
                    [
                      ( Text.pack "age"
                      , Schema.ObjectSchema Schema.emptySchema{Schema.schemaType = Just (NEL.singleton Schema.TypeNumber)}
                      )
                    ]
                )
          }
    obj = Aeson.Object (KeyMap.singleton (Key.fromText (Text.pack "age")) (Aeson.String (Text.pack "not a number")))
    errors = TestUtil.validatePure schema obj
   in
    assertSingleError errors $ \err -> do
      HUnit.assertEqual "keyword" (Just (Text.pack "type")) (Validate.errorKeyword err)
      HUnit.assertEqual "instance path" [Text.pack "age"] (Validate.errorInstancePath err)

testMinimumErrorDetails :: HUnit.Assertion
testMinimumErrorDetails =
  let
    schema = Schema.ObjectSchema Schema.emptySchema{Schema.schemaMinimum = Just 10}
    errors = TestUtil.validatePure schema (Aeson.Number 5)
   in
    assertSingleError errors $ \err -> do
      HUnit.assertEqual "keyword" (Just (Text.pack "minimum")) (Validate.errorKeyword err)
      HUnit.assertEqual "schema path" [Text.pack "minimum"] (Validate.errorSchemaPath err)

testFormatErrorDetails :: HUnit.Assertion
testFormatErrorDetails =
  let
    schema = Schema.ObjectSchema Schema.emptySchema{Schema.schemaFormat = Just (Text.pack "email")}
    registry =
      Validate.withFormatAssertion Validate.FormatAssertionAlways Validate.emptyRegistry
    errors = Validate.validate (Validate.compile registry schema) (Aeson.String (Text.pack "not-an-email"))
   in
    assertSingleError errors $ \err -> do
      HUnit.assertEqual "keyword" (Just (Text.pack "format")) (Validate.errorKeyword err)
      HUnit.assertEqual "schema path" [Text.pack "format"] (Validate.errorSchemaPath err)

testAnyOfErrorDetails :: HUnit.Assertion
testAnyOfErrorDetails =
  let
    schema =
      Schema.ObjectSchema
        Schema.emptySchema
          { Schema.schemaAnyOf =
              Just
                ( Schema.ObjectSchema Schema.emptySchema{Schema.schemaType = Just (NEL.singleton Schema.TypeString)}
                    NEL.:| [Schema.ObjectSchema Schema.emptySchema{Schema.schemaType = Just (NEL.singleton Schema.TypeNumber)}]
                )
          }
    errors = TestUtil.validatePure schema (Aeson.Bool True)
    isAnyOf e = case Validate.errorKind e of
      Validate.AnyOfFailed -> True
      _ -> False
   in
    HUnit.assertBool ("expected AnyOfFailed fault, got: " <> show errors) (any isAnyOf errors)

testOneOfErrorDetails :: HUnit.Assertion
testOneOfErrorDetails =
  let
    schema =
      Schema.ObjectSchema
        Schema.emptySchema
          { Schema.schemaOneOf =
              Just
                ( Schema.ObjectSchema Schema.emptySchema{Schema.schemaType = Just (NEL.singleton Schema.TypeString)}
                    NEL.:| [Schema.ObjectSchema Schema.emptySchema{Schema.schemaType = Just (NEL.singleton Schema.TypeNumber)}]
                )
          }
    errors = TestUtil.validatePure schema (Aeson.Bool True)
    isOneOf e = case Validate.errorKind e of
      Validate.OneOfFailed _ -> True
      _ -> False
   in
    HUnit.assertBool ("expected OneOfFailed fault, got: " <> show errors) (any isOneOf errors)

testNotErrorDetails :: HUnit.Assertion
testNotErrorDetails =
  let
    schema =
      Schema.ObjectSchema
        Schema.emptySchema
          { Schema.schemaNot =
              Just (Schema.ObjectSchema Schema.emptySchema{Schema.schemaType = Just (NEL.singleton Schema.TypeNumber)})
          }
    errors = TestUtil.validatePure schema (Aeson.Number 42)
   in
    assertSingleError errors $ \err -> do
      HUnit.assertEqual "keyword" (Just (Text.pack "not")) (Validate.errorKeyword err)
      HUnit.assertEqual "schema path" [Text.pack "not"] (Validate.errorSchemaPath err)

testIfThenErrorDetails :: HUnit.Assertion
testIfThenErrorDetails =
  let
    schema =
      Schema.ObjectSchema
        Schema.emptySchema
          { Schema.schemaIf =
              Just (Schema.ObjectSchema Schema.emptySchema{Schema.schemaType = Just (NEL.singleton Schema.TypeNumber)})
          , Schema.schemaThen = Just (Schema.ObjectSchema Schema.emptySchema{Schema.schemaMinimum = Just 10})
          , Schema.schemaElse =
              Just (Schema.ObjectSchema Schema.emptySchema{Schema.schemaType = Just (NEL.singleton Schema.TypeString)})
          }
    errors = TestUtil.validatePure schema (Aeson.Number 5)
    isMinimum e = case Validate.errorKind e of
      Validate.MinimumFailed _ _ -> True
      _ -> False
   in
    do
      HUnit.assertBool ("expected at least one error, got: " <> show errors) (not (null errors))
      HUnit.assertBool
        ("expected MinimumFailed fault under then, got: " <> show errors)
        (any isMinimum errors)

testDependentSchemasErrorDetails :: HUnit.Assertion
testDependentSchemasErrorDetails =
  let
    schema =
      Schema.ObjectSchema
        Schema.emptySchema
          { Schema.schemaDependentSchemas =
              Just
                ( OMap.fromList
                    [
                      ( Text.pack "foo"
                      , Schema.ObjectSchema
                          Schema.emptySchema{Schema.schemaRequired = Just (OSet.singleton (Text.pack "bar"))}
                      )
                    ]
                )
          }
    obj =
      Aeson.Object
        (KeyMap.singleton (Key.fromText (Text.pack "foo")) (Aeson.Number 1))
    errors = TestUtil.validatePure schema obj
    requiredOf e = case Validate.errorKind e of
      Validate.RequiredMissing k -> Just k
      _ -> Nothing
   in
    do
      HUnit.assertBool ("expected at least one error, got: " <> show errors) (not (null errors))
      HUnit.assertEqual
        ("expected RequiredMissing \"bar\" under dependentSchemas, got: " <> show errors)
        [Text.pack "bar"]
        (Maybe.mapMaybe requiredOf (Foldable.toList errors))

testDependentRequiredErrorDetails :: HUnit.Assertion
testDependentRequiredErrorDetails =
  let
    rawSchema =
      Aeson.Object
        ( KeyMap.singleton
            (Key.fromText (Text.pack "dependentRequired"))
            ( Aeson.Object
                ( KeyMap.singleton
                    (Key.fromText (Text.pack "foo"))
                    (Aeson.Array (Vector.fromList [Aeson.String (Text.pack "bar")]))
                )
            )
        )
    obj =
      Aeson.Object
        (KeyMap.singleton (Key.fromText (Text.pack "foo")) (Aeson.Number 1))
   in
    case TestUtil.decodeSchemaValue rawSchema of
      Left err -> HUnit.assertFailure ("schema decode failed: " <> err)
      Right schema ->
        let
          errors = TestUtil.validatePure schema obj
         in
          assertSingleError errors $ \err -> do
            HUnit.assertEqual "keyword" (Just (Text.pack "dependentRequired")) (Validate.errorKeyword err)
            HUnit.assertEqual
              "schema path"
              [Text.pack "dependentRequired"]
              (Validate.errorSchemaPath err)

testUnevaluatedItemsErrorDetails :: HUnit.Assertion
testUnevaluatedItemsErrorDetails =
  assertErrorLocation
    ( jsonObject
        [ (Text.pack "prefixItems", Aeson.Array (Vector.fromList [Aeson.Bool True]))
        , (Text.pack "unevaluatedItems", Aeson.Bool False)
        ]
    )
    (Aeson.Array (Vector.fromList [Aeson.String (Text.pack "a"), Aeson.Number 1]))
    Nothing
    [Text.pack "unevaluatedItems"]

testDeeplyNestedSchemaPath :: HUnit.Assertion
testDeeplyNestedSchemaPath =
  let
    nameSchema =
      Schema.ObjectSchema
        Schema.emptySchema{Schema.schemaType = Just (NEL.singleton Schema.TypeString), Schema.schemaMinLength = Just 1}
    userSchema =
      Schema.ObjectSchema
        Schema.emptySchema
          { Schema.schemaType = Just (NEL.singleton Schema.TypeObject)
          , Schema.schemaProperties = Just (OMap.fromList [(Text.pack "name", nameSchema)])
          }
    usersSchema =
      Schema.ObjectSchema
        Schema.emptySchema
          { Schema.schemaType = Just (NEL.singleton Schema.TypeArray)
          , Schema.schemaItems = Just userSchema
          }
    rootSchema =
      Schema.ObjectSchema
        Schema.emptySchema
          { Schema.schemaType = Just (NEL.singleton Schema.TypeObject)
          , Schema.schemaProperties = Just (OMap.fromList [(Text.pack "users", usersSchema)])
          }
    value =
      Aeson.Object
        ( KeyMap.singleton
            (Key.fromText (Text.pack "users"))
            ( Aeson.Array
                ( Vector.fromList
                    [ Aeson.Object
                        (KeyMap.singleton (Key.fromText (Text.pack "name")) (Aeson.String Text.empty))
                    ]
                )
            )
        )
    errors = TestUtil.validatePure rootSchema value
   in
    assertSingleError errors $ \err -> do
      HUnit.assertEqual "keyword" (Just (Text.pack "minLength")) (Validate.errorKeyword err)
      HUnit.assertEqual
        "instance path"
        (fmap Text.pack ["users", "0", "name"])
        (Validate.errorInstancePath err)
      HUnit.assertEqual
        "schema path"
        (fmap Text.pack ["properties", "users", "items", "properties", "name", "minLength"])
        (Validate.errorSchemaPath err)

testArrayItemInstancePath :: HUnit.Assertion
testArrayItemInstancePath =
  let
    schema =
      Schema.ObjectSchema
        Schema.emptySchema
          { Schema.schemaType = Just (NEL.singleton Schema.TypeArray)
          , Schema.schemaItems =
              Just (Schema.ObjectSchema Schema.emptySchema{Schema.schemaType = Just (NEL.singleton Schema.TypeNumber)})
          }
    value = Aeson.Array (Vector.fromList [Aeson.Number 1, Aeson.String (Text.pack "bad"), Aeson.Number 3])
    errors = TestUtil.validatePure schema value
   in
    assertSingleError errors $ \err -> do
      HUnit.assertEqual "instance path" [Text.pack "1"] (Validate.errorInstancePath err)
      HUnit.assertEqual "keyword" (Just (Text.pack "type")) (Validate.errorKeyword err)

testNestedObjectInstancePath :: HUnit.Assertion
testNestedObjectInstancePath =
  let
    schema =
      Schema.ObjectSchema
        Schema.emptySchema
          { Schema.schemaProperties =
              Just
                ( OMap.fromList
                    [
                      ( Text.pack "outer"
                      , Schema.ObjectSchema
                          Schema.emptySchema
                            { Schema.schemaProperties =
                                Just
                                  ( OMap.fromList
                                      [ (Text.pack "inner", Schema.ObjectSchema Schema.emptySchema{Schema.schemaType = Just (NEL.singleton Schema.TypeNumber)})
                                      ]
                                  )
                            }
                      )
                    ]
                )
          }
    value =
      Aeson.Object
        ( KeyMap.singleton
            (Key.fromText (Text.pack "outer"))
            ( Aeson.Object
                (KeyMap.singleton (Key.fromText (Text.pack "inner")) (Aeson.String (Text.pack "not a number")))
            )
        )
    errors = TestUtil.validatePure schema value
   in
    assertSingleError errors $ \err -> do
      HUnit.assertEqual "instance path" (fmap Text.pack ["outer", "inner"]) (Validate.errorInstancePath err)
      HUnit.assertEqual
        "schema path"
        (fmap Text.pack ["properties", "outer", "properties", "inner", "type"])
        (Validate.errorSchemaPath err)

testDeeplyNestedInstancePath :: HUnit.Assertion
testDeeplyNestedInstancePath =
  let
    leafSchema =
      Schema.ObjectSchema
        Schema.emptySchema{Schema.schemaType = Just (NEL.singleton Schema.TypeString), Schema.schemaMinLength = Just 1}
    mkObjLevel k inner =
      Schema.ObjectSchema
        Schema.emptySchema
          { Schema.schemaProperties = Just (OMap.fromList [(Text.pack k, inner)])
          }
    mkArrLevel inner =
      Schema.ObjectSchema
        Schema.emptySchema
          { Schema.schemaItems = Just inner
          }
    schema = mkObjLevel "a" (mkArrLevel (mkObjLevel "b" (mkArrLevel (mkObjLevel "c" leafSchema))))
    mkObj k v = Aeson.Object (KeyMap.singleton (Key.fromText (Text.pack k)) v)
    mkArr vs = Aeson.Array (Vector.fromList vs)
    value = mkObj "a" (mkArr [mkObj "b" (mkArr [mkObj "c" (Aeson.String Text.empty)])])
    errors = TestUtil.validatePure schema value
   in
    assertSingleError errors $ \err -> do
      HUnit.assertEqual
        "instance path"
        (fmap Text.pack ["a", "0", "b", "0", "c"])
        (Validate.errorInstancePath err)
      HUnit.assertEqual
        "schema path"
        (fmap Text.pack ["properties", "a", "items", "properties", "b", "items", "properties", "c", "minLength"])
        (Validate.errorSchemaPath err)

testAdditionalPropsSchemaPath :: HUnit.Assertion
testAdditionalPropsSchemaPath =
  let
    schema =
      Schema.ObjectSchema
        Schema.emptySchema
          { Schema.schemaProperties =
              Just
                ( OMap.fromList
                    [(Text.pack "name", Schema.ObjectSchema Schema.emptySchema{Schema.schemaType = Just (NEL.singleton Schema.TypeString)})]
                )
          , Schema.schemaAdditionalProperties = Just (Schema.BoolSchema False)
          }
    value =
      Aeson.Object
        ( KeyMap.fromList
            [ (Key.fromText (Text.pack "name"), Aeson.String (Text.pack "ok"))
            , (Key.fromText (Text.pack "extra"), Aeson.Number 1)
            ]
        )
    errors = TestUtil.validatePure schema value
   in
    assertSingleError errors $ \err -> do
      HUnit.assertEqual "instance path" [Text.pack "extra"] (Validate.errorInstancePath err)
      HUnit.assertBool
        "schema path starts with additionalProperties"
        ( case Validate.errorSchemaPath err of
            (s : _) | s == Text.pack "additionalProperties" -> True
            _ -> False
        )

testAllOfSchemaPath :: HUnit.Assertion
testAllOfSchemaPath =
  let
    schema =
      Schema.ObjectSchema
        Schema.emptySchema
          { Schema.schemaAllOf =
              Just
                ( Schema.ObjectSchema Schema.emptySchema{Schema.schemaType = Just (NEL.singleton Schema.TypeNumber)}
                    NEL.:| [Schema.ObjectSchema Schema.emptySchema{Schema.schemaMinimum = Just 10}]
                )
          }
    errors = TestUtil.validatePure schema (Aeson.Number 5)
   in
    do
      HUnit.assertEqual "error count" 1 (length errors)
      assertSingleError errors $ \err -> do
        HUnit.assertEqual "keyword" (Just (Text.pack "minimum")) (Validate.errorKeyword err)
        HUnit.assertEqual "schema path" (fmap Text.pack ["allOf", "1", "minimum"]) (Validate.errorSchemaPath err)

testPrefixItemsInstancePath :: HUnit.Assertion
testPrefixItemsInstancePath =
  let
    schema =
      Schema.ObjectSchema
        Schema.emptySchema
          { Schema.schemaPrefixItems =
              Just
                ( Schema.ObjectSchema Schema.emptySchema{Schema.schemaType = Just (NEL.singleton Schema.TypeString)}
                    NEL.:| [Schema.ObjectSchema Schema.emptySchema{Schema.schemaType = Just (NEL.singleton Schema.TypeNumber)}]
                )
          }
    value = Aeson.Array (Vector.fromList [Aeson.String (Text.pack "ok"), Aeson.String (Text.pack "should be number")])
    errors = TestUtil.validatePure schema value
   in
    assertSingleError errors $ \err -> do
      HUnit.assertEqual "instance path" [Text.pack "1"] (Validate.errorInstancePath err)
      HUnit.assertEqual "keyword" (Just (Text.pack "type")) (Validate.errorKeyword err)
      HUnit.assertEqual "schema path" (fmap Text.pack ["prefixItems", "1", "type"]) (Validate.errorSchemaPath err)

testIfElsePaths :: HUnit.Assertion
testIfElsePaths =
  let
    schema =
      Schema.ObjectSchema
        Schema.emptySchema
          { Schema.schemaIf =
              Just (Schema.ObjectSchema Schema.emptySchema{Schema.schemaType = Just (NEL.singleton Schema.TypeNumber)})
          , Schema.schemaThen = Just (Schema.ObjectSchema Schema.emptySchema{Schema.schemaMinimum = Just 0})
          , Schema.schemaElse =
              Just (Schema.ObjectSchema Schema.emptySchema{Schema.schemaType = Just (NEL.singleton Schema.TypeString)})
          }
    errors = TestUtil.validatePure schema (Aeson.Bool True)
   in
    do
      HUnit.assertBool "has error" (not (null errors))
      HUnit.assertBool
        "schema path contains else"
        (any (elem (Text.pack "else") . Validate.errorSchemaPath) errors)

testEnumFaultRenderQuotesValue :: HUnit.Assertion
testEnumFaultRenderQuotesValue =
  let
    schema =
      Schema.ObjectSchema
        Schema.emptySchema
          { Schema.schemaEnum =
              Just [Aeson.String (Text.pack "a"), Aeson.String (Text.pack "b")]
          }
    errors = TestUtil.validatePure schema (Aeson.String (Text.pack "c"))
   in
    assertSingleError errors $ \err -> do
      HUnit.assertEqual
        ("rendered message for input \"c\", got: " <> show (Validate.errorMessage err))
        (Text.pack "value not in enum: \"c\"")
        (Validate.errorMessage err)

testConstFaultRenderQuotesValues :: HUnit.Assertion
testConstFaultRenderQuotesValues =
  let
    schema = Schema.ObjectSchema Schema.emptySchema{Schema.schemaConst = Just (Aeson.Number 42)}
    errors = TestUtil.validatePure schema (Aeson.Number 7)
   in
    assertSingleError errors $ \err ->
      HUnit.assertEqual
        ("rendered message, got: " <> show (Validate.errorMessage err))
        (Text.pack "value 7 does not match const 42")
        (Validate.errorMessage err)

testAnyOfContext :: HUnit.Assertion
testAnyOfContext =
  let
    schema = stringOrNumberSchema (\branches -> Schema.emptySchema{Schema.schemaAnyOf = Just branches})
    errors = TestUtil.validatePure schema (Aeson.Bool True)
   in
    assertSingleError errors $ \err ->
      fmap Validate.errorSchemaPath (Validate.errorContext err)
        HUnit.@?= [fmap Text.pack ["anyOf", "0", "type"], fmap Text.pack ["anyOf", "1", "type"]]

stringOrNumberSchema :: (NEL.NonEmpty Schema.Schema -> Schema.SchemaObject) -> Schema.Schema
stringOrNumberSchema mkObj =
  Schema.ObjectSchema . mkObj $ typeSchema Schema.TypeString NEL.:| [typeSchema Schema.TypeNumber]

testOneOfZeroContext :: HUnit.Assertion
testOneOfZeroContext =
  let
    schema = stringOrNumberSchema (\branches -> Schema.emptySchema{Schema.schemaOneOf = Just branches})
    errors = TestUtil.validatePure schema (Aeson.Bool True)
   in
    assertSingleError errors $ \err -> do
      Validate.errorKind err HUnit.@?= Validate.OneOfFailed 0
      fmap Validate.errorKeyword (Validate.errorContext err) HUnit.@?= [Just (Text.pack "type"), Just (Text.pack "type")]

testOneOfManyContext :: HUnit.Assertion
testOneOfManyContext =
  let
    schema =
      Schema.ObjectSchema
        Schema.emptySchema{Schema.schemaOneOf = Just (Schema.BoolSchema True NEL.:| [Schema.BoolSchema True])}
    errors = TestUtil.validatePure schema Aeson.Null
   in
    assertSingleError errors $ \err -> do
      Validate.errorKind err HUnit.@?= Validate.OneOfFailed 2
      Validate.errorContext err HUnit.@?= []

testRenderJsonPointerEscapes :: HUnit.Assertion
testRenderJsonPointerEscapes =
  Validate.renderJsonPointer [Text.pack "a~/b", Text.pack "0"] HUnit.@?= Text.pack "/a~0~1b/0"

propRenderJsonPointerRoundTrip :: H.Property
propRenderJsonPointerRoundTrip = H.property $ do
  segments <- H.forAll (Gen.list (Range.linear 0 5) (Gen.text (Range.linear 0 6) (Gen.element ("ab~/01" :: String))))
  let
    rendered = Validate.renderJsonPointer segments
  fmap Prepass.unescapeJsonPointerSegment (drop 1 (Text.splitOn (Text.pack "/") rendered)) H.=== segments

testRenderErrorsNewlines :: HUnit.Assertion
testRenderErrorsNewlines = do
  let
    schema = Schema.ObjectSchema Schema.emptySchema{Schema.schemaRequired = Just (OSet.fromList [Text.pack "a", Text.pack "b"])}
    errors = TestUtil.validatePure schema (jsonObject [])
    newlines = Text.count (Text.pack "\n")
  Validate.renderErrors [] HUnit.@?= Text.empty
  newlines (Validate.renderErrors (take 1 errors)) HUnit.@?= 0
  newlines (Validate.renderErrors errors) HUnit.@?= 1

testRenderRootPath :: HUnit.Assertion
testRenderRootPath =
  assertSingleError (TestUtil.validatePure (typeSchema Schema.TypeString) Aeson.Null) $ \err ->
    HUnit.assertBool
      ("expected (root) prefix, got: " <> show (Validate.renderValidationError err))
      (Text.isPrefixOf (Text.pack "(root) [/type] ") (Validate.renderValidationError err))
