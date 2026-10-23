module CodecTests (tree) where

import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString.Lazy qualified as LBS
import Data.Foldable qualified as Foldable
import Data.Map.Ordered qualified as OMap
import Data.Maybe qualified as Maybe
import Fleece.Aeson qualified as FA
import Hedgehog qualified as H
import JsonSchema qualified as Schema
import JsonSchema.Fleece qualified as Fleece
import JsonSchema.OrderedDecoder qualified as OrderedDecoder
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit qualified as HUnit
import Test.Tasty.Hedgehog qualified as TastyH

import SchemaGen qualified
import TestUtil qualified

tree :: IO Tasty.TestTree
tree = do
  suiteFiles <- TestUtil.findJsonFiles (TestUtil.testSuitePath "tests/draft2020-12")
  remoteFiles <- TestUtil.findJsonFiles (TestUtil.testSuitePath "remotes")
  metaschemaFiles <- TestUtil.findJsonFiles "../json-schema-hs/data/metaschema/draft2020-12"
  pure
    $ Tasty.testGroup
      "codec agreement"
      [ TastyH.testProperty "encoders agree on generated schemas" propEncodersAgree
      , TastyH.testProperty "decoders agree on arbitrary JSON" propDecodersAgree
      , TastyH.testProperty "ordered decoder round-trips generated schemas" propOrderedDecoderRoundTrips
      , HUnit.testCase "ordered decoder keeps property order and the last duplicate" orderedDecoderKeepsOrder
      , HUnit.testCase "ordered decoder decodes a boolean document" orderedDecoderDecodesBoolean
      , HUnit.testCase "ordered decoder rejects trailing garbage" orderedDecoderRejectsTrailingGarbage
      , suiteFilesTests "test suite schemas" groupSchemas suiteFiles
      , suiteFilesTests "remote schemas" pure remoteFiles
      , suiteFilesTests "metaschemas" pure metaschemaFiles
      ]

propEncodersAgree :: H.Property
propEncodersAgree = H.property $ do
  schema <- H.forAll SchemaGen.genSchema
  Aeson.encode schema H.=== FA.encode Fleece.schemaFleeceSchema schema
  Aeson.toJSON schema H.=== FA.toValue Fleece.schemaFleeceSchema schema

propDecodersAgree :: H.Property
propDecodersAgree = H.property $ do
  value <- H.forAll SchemaGen.genJsonValue
  Foldable.for_ (codecDisagreement value) $ \msg -> H.footnote msg *> H.failure

codecDisagreement :: Aeson.Value -> Maybe String
codecDisagreement value =
  valueCodecDisagreement value <> orderedDecoderDisagreement (Aeson.encode value)

valueCodecDisagreement :: Aeson.Value -> Maybe String
valueCodecDisagreement value =
  case (TestUtil.decodeSchemaValue value, FA.fromValue Fleece.schemaFleeceSchema value) of
    (Left _, Left _) -> Nothing
    (Right aesonSchema, Right fleeceSchema)
      | aesonSchema /= fleeceSchema -> Just ("decoded schemas differ: " <> show (aesonSchema, fleeceSchema))
      | Aeson.encode aesonSchema /= FA.encode Fleece.schemaFleeceSchema fleeceSchema ->
          Just ("encodings differ for " <> show aesonSchema)
      | otherwise -> Nothing
    results -> Just ("decode results differ: " <> show results)

orderedDecoderDisagreement :: LBS.ByteString -> Maybe String
orderedDecoderDisagreement bytes =
  case (FA.decode Fleece.schemaFleeceSchema bytes, OrderedDecoder.decode Fleece.schemaFleeceSchema bytes) of
    (Left _, Left _) -> Nothing
    (Right aesonSchema, Right orderedSchema)
      | Aeson.toJSON aesonSchema /= Aeson.toJSON orderedSchema ->
          Just ("ordered decoder disagrees: " <> show (aesonSchema, orderedSchema))
      | otherwise -> Nothing
    results -> Just ("ordered decode results differ: " <> show results)

propOrderedDecoderRoundTrips :: H.Property
propOrderedDecoderRoundTrips = H.property $ do
  schema <- H.forAll SchemaGen.genSchema
  OrderedDecoder.decode Fleece.schemaFleeceSchema (FA.encode Fleece.schemaFleeceSchema schema) H.=== Right schema

orderedDecoderKeepsOrder :: HUnit.Assertion
orderedDecoderKeepsOrder = do
  schema <-
    either HUnit.assertFailure pure
      . OrderedDecoder.decode Fleece.schemaFleeceSchema
      $ "{\"properties\": {\"b\": true, \"c\": true, \"a\": true, \"b\": false}, \"$defs\": {\"z\": {}, \"y\": {}}}"
  case schema of
    Schema.ObjectSchema obj -> do
      fmap OMap.assocs (Schema.schemaProperties obj)
        HUnit.@?= Just [("b", Schema.BoolSchema False), ("c", Schema.BoolSchema True), ("a", Schema.BoolSchema True)]
      fmap (fmap fst . OMap.assocs) (Schema.schemaDefs obj) HUnit.@?= Just ["z", "y"]
    Schema.BoolSchema _ -> HUnit.assertFailure "expected an object schema"

orderedDecoderDecodesBoolean :: HUnit.Assertion
orderedDecoderDecodesBoolean =
  OrderedDecoder.decode Fleece.schemaFleeceSchema " true " HUnit.@?= Right (Schema.BoolSchema True)

orderedDecoderRejectsTrailingGarbage :: HUnit.Assertion
orderedDecoderRejectsTrailingGarbage =
  OrderedDecoder.decode Fleece.schemaFleeceSchema "{} {}" HUnit.@?= Left "Trailing garbage"

suiteFilesTests :: String -> (Aeson.Value -> [Aeson.Value]) -> [FilePath] -> Tasty.TestTree
suiteFilesTests name schemasOf paths =
  Tasty.testGroup name
    $ HUnit.testCase "files found" (HUnit.assertBool "no JSON files found" (not (null paths)))
      : fmap (suiteFileTest schemasOf) paths

suiteFileTest :: (Aeson.Value -> [Aeson.Value]) -> FilePath -> Tasty.TestTree
suiteFileTest schemasOf path =
  HUnit.testCase path $ do
    bytes <- LBS.readFile path
    document <- either (HUnit.assertFailure . (("parse " <> path <> ": ") <>)) pure (Aeson.eitherDecode bytes)
    Foldable.for_ (Maybe.mapMaybe codecDisagreement (schemasOf document)) HUnit.assertFailure

groupSchemas :: Aeson.Value -> [Aeson.Value]
groupSchemas document = case document of
  Aeson.Array groups -> foldMap groupSchema groups
  _ -> []
  where
    groupSchema group = case group of
      Aeson.Object obj -> Foldable.toList (KeyMap.lookup (Key.fromString "schema") obj)
      _ -> []
