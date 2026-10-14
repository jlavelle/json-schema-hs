module GenerateTests (generateTests) where

import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Int (Int32)
import Data.List.NonEmpty qualified as NEL
import Data.Map.Ordered qualified as OMap
import Data.Map.Strict qualified as Map
import Data.Scientific qualified as Scientific
import Data.Text qualified as Text
import Data.Vector qualified as Vector
import Fleece.Aeson qualified as FA
import Fleece.Core qualified as FC
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import JsonSchema qualified as Schema
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit qualified as HUnit
import Test.Tasty.Hedgehog qualified as TastyH
import TestUtil qualified

import JsonSchema.Generate qualified as Generate

generateTests :: Tasty.TestTree
generateTests =
  Tasty.testGroup
    "generate"
    [ Tasty.testGroup
        "primitives"
        [ HUnit.testCase "text" testTextSchema
        , HUnit.testCase "number" testNumberSchema
        , HUnit.testCase "boolean" testBooleanSchema
        , HUnit.testCase "null" testNullSchema
        , HUnit.testCase "int32" testInt32Schema
        , HUnit.testCase "double" testDoubleSchema
        , HUnit.testCase "describe" testDescribeSchema
        ]
    , Tasty.testGroup
        "objects"
        [ HUnit.testCase "required fields" testRequiredFields
        , HUnit.testCase "optional fields" testOptionalFields
        , HUnit.testCase "mixed fields" testMixedFields
        , HUnit.testCase "additional fields" testAdditionalFields
        ]
    , Tasty.testGroup
        "containers"
        [ HUnit.testCase "array of text" testArrayOfText
        , HUnit.testCase "nullable text" testNullableText
        , HUnit.testCase "list" testListSchema
        ]
    , Tasty.testGroup
        "constraints"
        [ boundCase "minLength" (schemaFor (FC.minLength 2 FC.text)) (Aeson.String "ab") (Aeson.String "a")
        , boundCase "maxLength" (schemaFor (FC.maxLength 2 FC.text)) (Aeson.String "ab") (Aeson.String "abc")
        , boundCase "minItems" (schemaFor (FC.minItems 2 (FC.list FC.text))) (textArray ["a", "b"]) (textArray ["a"])
        , boundCase "maxItems" (schemaFor (FC.maxItems 1 (FC.list FC.text))) (textArray ["a"]) (textArray ["a", "b"])
        , boundCase "minimum" (schemaFor (FC.minimum 3 FC.number)) (Aeson.Number 3) (Aeson.Number 2)
        , boundCase "maximum" (schemaFor (FC.maximum 3 FC.number)) (Aeson.Number 3) (Aeson.Number 4)
        , boundCase "nonEmpty" (schemaFor (FC.nonEmpty FC.text)) (textArray ["a"]) (textArray [])
        , HUnit.testCase "negative bound clamps to zero" testNegativeBoundClamps
        ]
    , Tasty.testGroup
        "nested"
        [ HUnit.testCase "nested object" testNestedObject
        , HUnit.testCase "array of objects" testArrayOfObjects
        , HUnit.testCase "deeply nested" testDeeplyNested
        ]
    , Tasty.testGroup
        "recursive"
        [ HUnit.testCase "recursive tree decode" testRecursiveTreeDecode
        , HUnit.testCase "recursive tree decode nested" testRecursiveTreeDecodeNested
        , HUnit.testCase "recursive tree generates defs" testRecursiveTreeGeneratesDefs
        , HUnit.testCase "recursive tree validates" testRecursiveTreeValidates
        , HUnit.testCase "recursive tree rejects invalid" testRecursiveTreeRejectsInvalid
        , TastyH.testProperty "recursive tree decode validates" propRecursiveDecodeValidates
        ]
    , Tasty.testGroup
        "unions"
        [ HUnit.testCase "nullable generates anyOf" testNullableGeneratesAnyOf
        ]
    , Tasty.testGroup
        "decode-validates"
        [ TastyH.testProperty "text decode validates" propTextDecodeValidates
        , TastyH.testProperty "number decode validates" propNumberDecodeValidates
        , TastyH.testProperty "object decode validates" propObjectDecodeValidates
        , TastyH.testProperty "nested object decode validates" propNestedDecodeValidates
        , TastyH.testProperty "array decode validates" propArrayDecodeValidates
        , TastyH.testProperty "nullable decode validates" propNullableDecodeValidates
        ]
    ]

testTextSchema :: HUnit.Assertion
testTextSchema = do
  let
    schema = schemaFor FC.text
  assertValidates "text validates string" schema (Aeson.String "hello")
  assertRejects "text rejects number" schema (Aeson.Number 42)

schemaFor :: FC.Schema Generate.JsonSchemaGen a -> Schema.Schema
schemaFor = Generate.toJsonSchema

assertValidates :: String -> Schema.Schema -> Aeson.Value -> HUnit.Assertion
assertValidates = TestUtil.assertValidates

assertRejects :: String -> Schema.Schema -> Aeson.Value -> HUnit.Assertion
assertRejects = TestUtil.assertRejects

testNumberSchema :: HUnit.Assertion
testNumberSchema = do
  let
    schema = schemaFor FC.number
  assertValidates "number validates number" schema (Aeson.Number 3.14)
  assertRejects "number rejects string" schema (Aeson.String "x")

testBooleanSchema :: HUnit.Assertion
testBooleanSchema = do
  let
    schema = schemaFor FC.boolean
  assertValidates "boolean validates bool" schema (Aeson.Bool True)
  assertRejects "boolean rejects number" schema (Aeson.Number 1)

testNullSchema :: HUnit.Assertion
testNullSchema = do
  let
    schema = schemaFor FC.null
  assertValidates "null validates null" schema Aeson.Null
  assertRejects "null rejects string" schema (Aeson.String "x")

testDescribeSchema :: HUnit.Assertion
testDescribeSchema = do
  let
    schema = schemaFor (FC.describe "A name" FC.text)
  case schema of
    Schema.ObjectSchema obj -> do
      HUnit.assertEqual "describe emits description" (Just "A name") (Schema.schemaDescription obj)
      HUnit.assertEqual "describe keeps type" (Just (NEL.singleton Schema.TypeString)) (Schema.schemaType obj)
    Schema.BoolSchema _ -> HUnit.assertFailure "expected object schema"

testInt32Schema :: HUnit.Assertion
testInt32Schema = do
  let
    schema = schemaFor FC.int32
  case schema of
    Schema.ObjectSchema obj -> do
      HUnit.assertEqual
        "int32 emits integer type"
        (Just (NEL.singleton Schema.TypeInteger))
        (Schema.schemaType obj)
      HUnit.assertEqual
        "int32 minimum is Int32 lower bound"
        (Just (fromInteger (toInteger (minBound :: Int32))))
        (Schema.schemaMinimum obj)
      HUnit.assertEqual
        "int32 maximum is Int32 upper bound"
        (Just (fromInteger (toInteger (maxBound :: Int32))))
        (Schema.schemaMaximum obj)
      HUnit.assertEqual "int32 emits int32 format" (Just "int32") (Schema.schemaFormat obj)
    _ -> HUnit.assertFailure "expected ObjectSchema"
  assertValidates "int32 validates 42" schema (Aeson.Number 42)
  assertValidates
    "int32 validates 2^31-1 (boundary, in range)"
    schema
    (Aeson.Number 2147483647)
  assertRejects
    "int32 rejects 2^31 (above maximum)"
    schema
    (Aeson.Number 2147483648)
  assertRejects
    "int32 rejects -2^31-1 (below minimum)"
    schema
    (Aeson.Number (-2147483649))
  assertRejects
    "int32 rejects 1.5 (not an integer)"
    schema
    (Aeson.Number 1.5)

testDoubleSchema :: HUnit.Assertion
testDoubleSchema =
  case schemaFor FC.double of
    Schema.ObjectSchema obj -> do
      HUnit.assertEqual "double emits number type" (Just (NEL.singleton Schema.TypeNumber)) (Schema.schemaType obj)
      HUnit.assertEqual "double emits double format" (Just "double") (Schema.schemaFormat obj)
    _ -> HUnit.assertFailure "expected ObjectSchema"

testRequiredFields :: HUnit.Assertion
testRequiredFields = do
  let
    schema = schemaFor personSchema
    goodValue =
      Aeson.Object
        $ KeyMap.fromList
          [ (Key.fromText "name", Aeson.String "Alice")
          , (Key.fromText "age", Aeson.Number 30)
          ]
    missingAge =
      Aeson.Object
        $ KeyMap.singleton (Key.fromText "name") (Aeson.String "Alice")
  assertValidates "valid object validates" schema goodValue
  assertRejects "missing field rejects" schema missingAge

personSchema :: (FC.Fleece t) => FC.Schema t Person
personSchema =
  FC.object
    $ FC.constructor Person
      FC.#+ FC.required "name" personName FC.text
      FC.#+ FC.required "age" personAge FC.number

data Person = Person
  { personName :: Text.Text
  , personAge :: Scientific.Scientific
  }

testOptionalFields :: HUnit.Assertion
testOptionalFields = do
  let
    schema = schemaFor withOptionalSchema
    withBoth =
      Aeson.Object
        $ KeyMap.fromList
          [ (Key.fromText "required", Aeson.String "yes")
          , (Key.fromText "optional", Aeson.String "maybe")
          ]
    withoutOptional =
      Aeson.Object
        $ KeyMap.singleton (Key.fromText "required") (Aeson.String "yes")
  assertValidates "with both validates" schema withBoth
  assertValidates "without optional validates" schema withoutOptional

withOptionalSchema :: (FC.Fleece t) => FC.Schema t WithOptional
withOptionalSchema =
  FC.object
    $ FC.constructor WithOptional
      FC.#+ FC.required "required" woRequired FC.text
      FC.#+ FC.optional "optional" woOptional FC.text

data WithOptional = WithOptional
  { woRequired :: Text.Text
  , woOptional :: Maybe Text.Text
  }

testMixedFields :: HUnit.Assertion
testMixedFields = do
  let
    schema = schemaFor withOptionalSchema
    missingRequired =
      Aeson.Object
        $ KeyMap.singleton (Key.fromText "optional") (Aeson.String "oops")
  assertRejects "missing required rejects" schema missingRequired

testAdditionalFields :: HUnit.Assertion
testAdditionalFields = do
  let
    schema = schemaFor withAdditionalSchema
    good =
      Aeson.Object
        $ KeyMap.fromList
          [ (Key.fromText "known", Aeson.String "yes")
          , (Key.fromText "extra1", Aeson.Number 1)
          ]
    badAdditional =
      Aeson.Object
        $ KeyMap.fromList
          [ (Key.fromText "known", Aeson.String "yes")
          , (Key.fromText "extra1", Aeson.String "not a number")
          ]
  assertValidates "valid additional validates" schema good
  assertRejects "wrong type additional rejects" schema badAdditional

withAdditionalSchema :: (FC.Fleece t) => FC.Schema t WithAdditional
withAdditionalSchema =
  FC.object
    $ FC.constructor WithAdditional
      FC.#+ FC.required "known" waKnown FC.text
      FC.#* FC.additionalFields waExtra FC.number

data WithAdditional = WithAdditional
  { waKnown :: Text.Text
  , waExtra :: Map.Map Text.Text Scientific.Scientific
  }

testArrayOfText :: HUnit.Assertion
testArrayOfText = do
  let
    schema = schemaFor (FC.array FC.text)
    good = textArray ["a", "b"]
    bad = Aeson.Array $ Vector.fromList [Aeson.String "a", Aeson.Number 1]
  assertValidates "array of strings validates" schema good
  assertRejects "mixed array rejects" schema bad

textArray :: [Text.Text] -> Aeson.Value
textArray = Aeson.Array . Vector.fromList . fmap Aeson.String

testNullableText :: HUnit.Assertion
testNullableText = do
  let
    schema = schemaFor (FC.nullable FC.text)
  assertValidates "string validates" schema (Aeson.String "hello")
  assertValidates "null validates" schema Aeson.Null
  assertRejects "number rejects" schema (Aeson.Number 1)

testListSchema :: HUnit.Assertion
testListSchema = do
  let
    schema = schemaFor (FC.list FC.number)
    good = Aeson.Array $ Vector.fromList [Aeson.Number 1, Aeson.Number 2]
  assertValidates "list validates" schema good

boundCase :: String -> Schema.Schema -> Aeson.Value -> Aeson.Value -> Tasty.TestTree
boundCase name schema within outside =
  HUnit.testCase name $ do
    assertValidates "value within bound validates" schema within
    assertRejects "value outside bound rejects" schema outside

testNegativeBoundClamps :: HUnit.Assertion
testNegativeBoundClamps =
  assertValidates "empty string validates" (schemaFor (FC.minLength (-1) FC.text)) (Aeson.String "")

testNestedObject :: HUnit.Assertion
testNestedObject = do
  let
    schema = schemaFor personWithAddressSchema
    good =
      Aeson.Object
        $ KeyMap.fromList
          [ (Key.fromText "name", Aeson.String "Alice")
          ,
            ( Key.fromText "address"
            , Aeson.Object
                $ KeyMap.fromList
                  [ (Key.fromText "street", Aeson.String "123 Main")
                  , (Key.fromText "city", Aeson.String "Springfield")
                  ]
            )
          ]
    badNested =
      Aeson.Object
        $ KeyMap.fromList
          [ (Key.fromText "name", Aeson.String "Alice")
          ,
            ( Key.fromText "address"
            , Aeson.Object
                $ KeyMap.singleton (Key.fromText "street") (Aeson.String "123 Main")
            )
          ]
  assertValidates "valid nested validates" schema good
  assertRejects "missing nested field rejects" schema badNested

personWithAddressSchema :: (FC.Fleece t) => FC.Schema t PersonWithAddress
personWithAddressSchema =
  FC.object
    $ FC.constructor PersonWithAddress
      FC.#+ FC.required "name" pwaName FC.text
      FC.#+ FC.required "address" pwaAddress addressSchema

data PersonWithAddress = PersonWithAddress
  { pwaName :: Text.Text
  , pwaAddress :: Address
  }

data Address = Address
  { addrStreet :: Text.Text
  , addrCity :: Text.Text
  }

addressSchema :: (FC.Fleece t) => FC.Schema t Address
addressSchema =
  FC.object
    $ FC.constructor Address
      FC.#+ FC.required "street" addrStreet FC.text
      FC.#+ FC.required "city" addrCity FC.text

testArrayOfObjects :: HUnit.Assertion
testArrayOfObjects = do
  let
    schema = schemaFor (FC.array addressSchema)
    good =
      Aeson.Array
        $ Vector.fromList
          [ Aeson.Object
              $ KeyMap.fromList
                [ (Key.fromText "street", Aeson.String "123 Main")
                , (Key.fromText "city", Aeson.String "Springfield")
                ]
          ]
    bad =
      Aeson.Array
        $ Vector.fromList
          [Aeson.Object $ KeyMap.singleton (Key.fromText "street") (Aeson.String "123 Main")]
  assertValidates "array of objects validates" schema good
  assertRejects "invalid object in array rejects" schema bad

testDeeplyNested :: HUnit.Assertion
testDeeplyNested = do
  let
    schema = schemaFor outerSchema
    good =
      Aeson.Object
        $ KeyMap.fromList
          [ (Key.fromText "label", Aeson.String "outer")
          ,
            ( Key.fromText "inner"
            , Aeson.Object
                $ KeyMap.fromList
                  [ (Key.fromText "value", Aeson.Number 42)
                  ,
                    ( Key.fromText "detail"
                    , Aeson.Object
                        $ KeyMap.singleton (Key.fromText "note") (Aeson.String "hi")
                    )
                  ]
            )
          ]
    badDeep =
      Aeson.Object
        $ KeyMap.fromList
          [ (Key.fromText "label", Aeson.String "outer")
          ,
            ( Key.fromText "inner"
            , Aeson.Object
                $ KeyMap.fromList
                  [ (Key.fromText "value", Aeson.Number 42)
                  , (Key.fromText "detail", Aeson.Object KeyMap.empty)
                  ]
            )
          ]
  assertValidates "deeply nested validates" schema good
  assertRejects "deeply nested missing rejects" schema badDeep

outerSchema :: (FC.Fleece t) => FC.Schema t Outer
outerSchema =
  FC.object
    $ FC.constructor Outer
      FC.#+ FC.required "label" outerLabel FC.text
      FC.#+ FC.required "inner" outerInner innerSchema

data Outer = Outer
  { outerLabel :: Text.Text
  , outerInner :: Inner
  }

data Inner = Inner
  { innerValue :: Scientific.Scientific
  , innerDetail :: Detail
  }

newtype Detail = Detail
  { detailNote :: Text.Text
  }

innerSchema :: (FC.Fleece t) => FC.Schema t Inner
innerSchema =
  FC.object
    $ FC.constructor Inner
      FC.#+ FC.required "value" innerValue FC.number
      FC.#+ FC.required "detail" innerDetail detailSchema

detailSchema :: (FC.Fleece t) => FC.Schema t Detail
detailSchema =
  FC.object
    $ FC.constructor Detail
      FC.#+ FC.required "note" detailNote FC.text

testRecursiveTreeDecode :: HUnit.Assertion
testRecursiveTreeDecode = do
  let
    treeJson =
      Aeson.Object
        $ KeyMap.fromList
          [ (Key.fromText "value", Aeson.String "root")
          ,
            ( Key.fromText "children"
            , Aeson.Array
                $ Vector.singleton
                  ( Aeson.Object
                      $ KeyMap.fromList
                        [ (Key.fromText "value", Aeson.String "child")
                        , (Key.fromText "children", Aeson.Array Vector.empty)
                        ]
                  )
            )
          ]
  case decodeWith treeSchema treeJson of
    Left err -> HUnit.assertFailure ("decode failed: " <> err)
    Right tree -> do
      HUnit.assertEqual "root value" "root" (treeValue tree)
      case treeChildren tree of
        [child] -> do
          HUnit.assertEqual "child value" "child" (treeValue child)
          HUnit.assertEqual "grandchildren" [] (treeChildren child)
        other -> HUnit.assertFailure ("expected 1 child, got " <> show (length other))

data Tree = Tree
  { treeValue :: Text.Text
  , treeChildren :: [Tree]
  }
  deriving (Eq, Show)

decodeWith :: FC.Schema FA.Decoder a -> Aeson.Value -> Either String a
decodeWith = FA.fromValue

treeSchema :: (FC.Fleece t) => FC.Schema t Tree
treeSchema =
  FC.object
    $ FC.constructor Tree
      FC.#+ FC.required "value" treeValue FC.text
      FC.#+ FC.required "children" treeChildren (FC.list treeSchema)

testRecursiveTreeDecodeNested :: HUnit.Assertion
testRecursiveTreeDecodeNested = do
  let
    treeJson =
      Aeson.Object
        $ KeyMap.fromList
          [ (Key.fromText "value", Aeson.String "root")
          ,
            ( Key.fromText "children"
            , Aeson.Array
                $ Vector.fromList
                  [ Aeson.Object
                      $ KeyMap.fromList
                        [ (Key.fromText "value", Aeson.String "a")
                        ,
                          ( Key.fromText "children"
                          , Aeson.Array
                              $ Vector.singleton
                                ( Aeson.Object
                                    $ KeyMap.fromList
                                      [ (Key.fromText "value", Aeson.String "b")
                                      , (Key.fromText "children", Aeson.Array Vector.empty)
                                      ]
                                )
                          )
                        ]
                  , Aeson.Object
                      $ KeyMap.fromList
                        [ (Key.fromText "value", Aeson.String "c")
                        , (Key.fromText "children", Aeson.Array Vector.empty)
                        ]
                  ]
            )
          ]
  case decodeWith treeSchema treeJson of
    Left err -> HUnit.assertFailure ("decode failed: " <> err)
    Right tree -> do
      HUnit.assertEqual "root" "root" (treeValue tree)
      HUnit.assertEqual "child count" 2 (length (treeChildren tree))
      case treeChildren tree of
        [a, c] -> do
          HUnit.assertEqual "first child" "a" (treeValue a)
          HUnit.assertEqual "grandchild count" 1 (length (treeChildren a))
          HUnit.assertEqual "second child" "c" (treeValue c)
          HUnit.assertEqual "second child leaves" [] (treeChildren c)
        _ -> HUnit.assertFailure "unexpected children structure"

testRecursiveTreeGeneratesDefs :: HUnit.Assertion
testRecursiveTreeGeneratesDefs = do
  let
    schema = schemaFor treeSchema
    expectedKey = Text.pack "GenerateTests.Tree"
  case schema of
    Schema.ObjectSchema obj ->
      case Schema.schemaDefs obj of
        Just defs ->
          let
            defKeys = fmap fst (OMap.assocs defs)
           in
            HUnit.assertBool
              ( "expected $defs to contain "
                  <> Text.unpack expectedKey
                  <> ", got keys: "
                  <> show defKeys
              )
              (expectedKey `elem` defKeys)
        Nothing -> HUnit.assertFailure "expected $defs in recursive schema"
    _ -> HUnit.assertFailure "expected ObjectSchema"

testRecursiveTreeValidates :: HUnit.Assertion
testRecursiveTreeValidates = do
  let
    schema = schemaFor treeSchema
    leaf =
      Aeson.Object
        $ KeyMap.fromList
          [ (Key.fromText "value", Aeson.String "leaf")
          , (Key.fromText "children", Aeson.Array Vector.empty)
          ]
    nested =
      Aeson.Object
        $ KeyMap.fromList
          [ (Key.fromText "value", Aeson.String "root")
          ,
            ( Key.fromText "children"
            , Aeson.Array
                $ Vector.fromList
                  [ Aeson.Object
                      $ KeyMap.fromList
                        [ (Key.fromText "value", Aeson.String "child")
                        ,
                          ( Key.fromText "children"
                          , Aeson.Array
                              $ Vector.singleton
                                ( Aeson.Object
                                    $ KeyMap.fromList
                                      [ (Key.fromText "value", Aeson.String "grandchild")
                                      , (Key.fromText "children", Aeson.Array Vector.empty)
                                      ]
                                )
                          )
                        ]
                  ]
            )
          ]
  assertValidates "leaf validates" schema leaf
  assertValidates "nested tree validates" schema nested

testRecursiveTreeRejectsInvalid :: HUnit.Assertion
testRecursiveTreeRejectsInvalid = do
  let
    schema = schemaFor treeSchema
    missingChildren =
      Aeson.Object
        $ KeyMap.fromList
          [ (Key.fromText "value", Aeson.String "root")
          ,
            ( Key.fromText "children"
            , Aeson.Array
                $ Vector.singleton
                  ( Aeson.Object
                      $ KeyMap.singleton (Key.fromText "value") (Aeson.String "bad")
                  )
            )
          ]
    wrongChildType =
      Aeson.Object
        $ KeyMap.fromList
          [ (Key.fromText "value", Aeson.String "root")
          , (Key.fromText "children", Aeson.String "not an array")
          ]
  assertRejects "missing children in nested rejects" schema missingChildren
  assertRejects "wrong children type rejects" schema wrongChildType

propRecursiveDecodeValidates :: H.Property
propRecursiveDecodeValidates = H.property $ do
  tree <- H.forAll genTree
  let
    jsonVal = treeToJson tree
    jsonSchema = schemaFor treeSchema
  case decodeWith treeSchema jsonVal of
    Left err -> H.annotate err *> H.failure
    Right _ -> validatesProp jsonSchema jsonVal >>= H.assert

genTree :: H.Gen Tree
genTree =
  Gen.recursive
    Gen.choice
    [Tree <$> Gen.text (Range.linear 1 10) Gen.alpha <*> pure []]
    [ do
        val <- Gen.text (Range.linear 1 10) Gen.alpha
        children <- Gen.list (Range.linear 0 3) genTree
        pure (Tree val children)
    ]

treeToJson :: Tree -> Aeson.Value
treeToJson t =
  Aeson.Object
    $ KeyMap.fromList
      [ (Key.fromText "value", Aeson.String (treeValue t))
      , (Key.fromText "children", Aeson.Array . Vector.fromList . fmap treeToJson $ treeChildren t)
      ]

validatesProp :: Schema.Schema -> Aeson.Value -> H.PropertyT IO Bool
validatesProp = TestUtil.validatesProp

testNullableGeneratesAnyOf :: HUnit.Assertion
testNullableGeneratesAnyOf = do
  let
    schema = schemaFor (FC.nullable FC.text)
    nullArm = Schema.ObjectSchema Schema.emptySchema{Schema.schemaType = Just (NEL.singleton Schema.TypeNull)}
    wrappedArm = schemaFor FC.text
  case schema of
    Schema.ObjectSchema obj ->
      case Schema.schemaAnyOf obj of
        Just arms -> do
          HUnit.assertEqual ("anyOf arm count, got " <> show arms) 2 (length arms)
          HUnit.assertBool
            ("expected a {type:null} arm in " <> show arms)
            (nullArm `elem` arms)
          HUnit.assertBool
            ("expected a wrapped-text arm equal to " <> show wrappedArm <> " in " <> show arms)
            (wrappedArm `elem` arms)
        other -> HUnit.assertFailure ("expected anyOf with 2 elements, got " <> show other)
    _ -> HUnit.assertFailure "expected ObjectSchema"

propTextDecodeValidates :: H.Property
propTextDecodeValidates = H.property $ do
  s <- H.forAll $ Gen.text (Range.linear 0 50) Gen.alphaNum
  let
    jsonVal = Aeson.String s
    jsonSchema = schemaFor FC.text
  case decodeWith FC.text jsonVal of
    Left _ -> H.failure
    Right _ -> validatesProp jsonSchema jsonVal >>= H.assert

propNumberDecodeValidates :: H.Property
propNumberDecodeValidates = H.property $ do
  n <- H.forAll $ Scientific.fromFloatDigits <$> Gen.double (Range.linearFrac (-1000) 1000)
  let
    jsonVal = Aeson.Number n
    jsonSchema = schemaFor FC.number
  case decodeWith FC.number jsonVal of
    Left _ -> H.failure
    Right _ -> validatesProp jsonSchema jsonVal >>= H.assert

propObjectDecodeValidates :: H.Property
propObjectDecodeValidates = H.property $ do
  name <- H.forAll $ Gen.text (Range.linear 1 20) Gen.alpha
  age <- H.forAll $ Scientific.fromFloatDigits <$> Gen.double (Range.linearFrac 0 150)
  let
    jsonVal =
      Aeson.Object
        $ KeyMap.fromList
          [ (Key.fromText "name", Aeson.String name)
          , (Key.fromText "age", Aeson.Number age)
          ]
    jsonSchema = schemaFor personSchema
  case decodeWith personSchema jsonVal of
    Left err -> H.annotate err *> H.failure
    Right _ -> validatesProp jsonSchema jsonVal >>= H.assert

propNestedDecodeValidates :: H.Property
propNestedDecodeValidates = H.property $ do
  name <- H.forAll $ Gen.text (Range.linear 1 20) Gen.alpha
  street <- H.forAll $ Gen.text (Range.linear 1 30) Gen.alpha
  city <- H.forAll $ Gen.text (Range.linear 1 20) Gen.alpha
  let
    jsonVal =
      Aeson.Object
        $ KeyMap.fromList
          [ (Key.fromText "name", Aeson.String name)
          ,
            ( Key.fromText "address"
            , Aeson.Object
                $ KeyMap.fromList
                  [ (Key.fromText "street", Aeson.String street)
                  , (Key.fromText "city", Aeson.String city)
                  ]
            )
          ]
    jsonSchema = schemaFor personWithAddressSchema
  case decodeWith personWithAddressSchema jsonVal of
    Left err -> H.annotate err *> H.failure
    Right _ -> validatesProp jsonSchema jsonVal >>= H.assert

propArrayDecodeValidates :: H.Property
propArrayDecodeValidates = H.property $ do
  strings <- H.forAll $ Gen.list (Range.linear 0 10) (Gen.text (Range.linear 0 20) Gen.alphaNum)
  let
    listSchema :: (FC.Fleece t) => FC.Schema t [Text.Text]
    listSchema = FC.list FC.text
    jsonVal = Aeson.Array . Vector.fromList . fmap Aeson.String $ strings
    jsonSchema = schemaFor listSchema
  case decodeWith listSchema jsonVal of
    Left err -> H.annotate err *> H.failure
    Right _ -> validatesProp jsonSchema jsonVal >>= H.assert

propNullableDecodeValidates :: H.Property
propNullableDecodeValidates = H.property $ do
  isNull <- H.forAll Gen.bool
  payload <- H.forAll $ Gen.text (Range.linear 0 30) Gen.unicode
  let
    nullableSchema :: (FC.Fleece t) => FC.Schema t (Either FC.Null Text.Text)
    nullableSchema = FC.nullable FC.text
    jsonVal = if isNull then Aeson.Null else Aeson.String payload
    jsonSchema = schemaFor nullableSchema
  case decodeWith nullableSchema jsonVal of
    Left err -> H.annotate err *> H.failure
    Right _ -> validatesProp jsonSchema jsonVal >>= H.assert
