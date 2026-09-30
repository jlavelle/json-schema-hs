module JsonSchemaGoldenTest (tree) where

import Data.Aeson.Encode.Pretty qualified as AesonPretty
import Fleece.Aeson qualified as FA
import Test.Tasty qualified as Tasty
import Test.Tasty.Golden qualified as TastyG

import JsonSchema.Fleece qualified as Fleece
import JsonSchema.Generate qualified as Generate

tree :: Tasty.TestTree
tree =
  let
    goldenPath = "fleece-test/golden/meta-schema.json"
   in
    Tasty.testGroup
      "golden"
      [ TastyG.goldenVsString "meta-schema" goldenPath
          . pure
          . (<> "\n")
          . AesonPretty.encodePretty
          . FA.toValue Fleece.schemaFleeceSchema
          $ Generate.toJsonSchema Fleece.schemaFleeceSchema
      ]
