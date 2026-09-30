module Main (main) where

import Test.Tasty qualified as Tasty

import CodecTests qualified
import GenerateTests qualified
import JsonSchemaGoldenTest qualified

main :: IO ()
main = do
  codecTests <- CodecTests.tree
  Tasty.defaultMain
    $ Tasty.testGroup
      "json-schema-fleece"
      [ codecTests
      , GenerateTests.generateTests
      , JsonSchemaGoldenTest.tree
      ]
