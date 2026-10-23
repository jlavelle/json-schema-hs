module Main (main) where

import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString.Lazy qualified as LBS
import Data.List.NonEmpty qualified as NEL
import Data.Map.Ordered qualified as OMap
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Vector qualified as Vector
import Test.Tasty.Bench qualified as Bench

import Bench.Common qualified as Common
import JsonSchema qualified as Schema
import JsonSchema.Validate qualified as Validate

main :: IO ()
main =
  Bench.defaultMain
    [ Bench.bgroup
        "array"
        [ scaling "items" $ \n ->
            Bench.nf (Common.compileAndValidate Validate.emptyRegistry itemsSchema) (mkNumbers n)
        , scaling "uniqueItems" $ \n ->
            Bench.nf (Common.compileAndValidate Validate.emptyRegistry uniqueItemsSchema) (mkNumbers n)
        , scaling "uniqueItems-almost" $ \n ->
            Bench.nf (Common.compileAndValidate Validate.emptyRegistry uniqueItemsSchema) (mkNumbersAlmostUnique n)
        , scaling "prefixItems" $ \n ->
            Bench.nf (Common.compileAndValidate Validate.emptyRegistry (mkPrefixItemsSchema n)) (mkNumbers n)
        , scaling "contains" $ \n ->
            Bench.nf (Common.compileAndValidate Validate.emptyRegistry (mkContainsSchema n)) (mkNumbers n)
        ]
    , Bench.bgroup
        "object"
        [ scaling "properties" $ \n ->
            Bench.nf (Common.compileAndValidate Validate.emptyRegistry (mkPropertiesSchema n)) (mkNumericObject n)
        , scaling "patternProperties" $ \n ->
            Bench.nf (Common.compileAndValidate Validate.emptyRegistry Common.patternPropsSchema) (mkNumericObject n)
        , scaling "patternProperties-array" $ \n ->
            Bench.nf (Common.compileAndValidate Validate.emptyRegistry Common.patternPropsArraySchema) (mkPatternPropObjectArray n)
        , scaling "additionalProperties" $ \n ->
            Bench.nf (Common.compileAndValidate Validate.emptyRegistry (mkAdditionalPropsSchema n)) (mkNumericObject n)
        , scaling "propertyNames" $ \n ->
            Bench.nf (Common.compileAndValidate Validate.emptyRegistry Common.propertyNamesSchema) (mkNumericObject n)
        , scaling "dependentSchemas" $ \n ->
            Bench.nf (Common.compileAndValidate Validate.emptyRegistry (mkDependentSchemasSchema n)) (mkNumericObject n)
        , scaling "nested" $ \n ->
            Bench.nf (Common.compileAndValidate Validate.emptyRegistry (mkNestedSchema n)) (mkNestedValue n)
        , scaling "records" $ \n ->
            Bench.nf (Common.compileAndValidate Validate.emptyRegistry Common.recordArraySchema) (mkRecords n)
        , scaling "unevaluatedProperties" $ \n ->
            Bench.nf (Common.compileAndValidate Validate.emptyRegistry (mkUnevaluatedPropsSchema n)) (mkNumericObject n)
        , scaling "unevaluatedItems" $ \n ->
            Bench.nf (Common.compileAndValidate Validate.emptyRegistry unevaluatedItemsSchema) (mkNumbers n)
        ]
    , Bench.bgroup
        "composition"
        [ scaling "allOf" $ \n ->
            Bench.nf (Common.compileAndValidate Validate.emptyRegistry allOfSchema) (mkNumbers n)
        , scaling "anyOf" $ \n ->
            Bench.nf (Common.compileAndValidate Validate.emptyRegistry (mkAnyOfSchema n)) (mkNumbers n)
        , scaling "oneOf" $ \n ->
            Bench.nf (Common.compileAndValidate Validate.emptyRegistry (mkOneOfSchema n)) (mkNumbers n)
        , scaling "not" $ \n ->
            Bench.nf (Common.compileAndValidate Validate.emptyRegistry Common.notArraySchema) (mkNumbers n)
        , scaling "if/then/else" $ \n ->
            Bench.nf (Common.compileAndValidate Validate.emptyRegistry Common.conditionalArraySchema) (mkNumbers n)
        , scaling "nested allOf" $ \n ->
            Bench.nf (Common.compileAndValidate Validate.emptyRegistry nestedAllOfSchema) (mkNumbers n)
        ]
    , Bench.bgroup
        "format"
        [ scaling "date-time" $ \n ->
            Bench.nf (Common.compileAndValidate Validate.emptyRegistry (Common.formatArraySchema "date-time")) (mkDateTimes n)
        , scaling "date" $ \n ->
            Bench.nf (Common.compileAndValidate Validate.emptyRegistry (Common.formatArraySchema "date")) (mkDates n)
        , scaling "time" $ \n ->
            Bench.nf (Common.compileAndValidate Validate.emptyRegistry (Common.formatArraySchema "time")) (mkTimes n)
        , scaling "email" $ \n ->
            Bench.nf (Common.compileAndValidate Validate.emptyRegistry (Common.formatArraySchema "email")) (mkEmails n)
        , scaling "uri" $ \n ->
            Bench.nf (Common.compileAndValidate Validate.emptyRegistry (Common.formatArraySchema "uri")) (mkUris n)
        , scaling "ipv4" $ \n ->
            Bench.nf (Common.compileAndValidate Validate.emptyRegistry (Common.formatArraySchema "ipv4")) (mkIpv4s n)
        , scaling "ipv6" $ \n ->
            Bench.nf (Common.compileAndValidate Validate.emptyRegistry (Common.formatArraySchema "ipv6")) (mkIpv6s n)
        , scaling "uuid" $ \n ->
            Bench.nf (Common.compileAndValidate Validate.emptyRegistry (Common.formatArraySchema "uuid")) (mkUuids n)
        , scaling "hostname" $ \n ->
            Bench.nf (Common.compileAndValidate Validate.emptyRegistry (Common.formatArraySchema "hostname")) (mkHostnames n)
        , scaling "duration" $ \n ->
            Bench.nf (Common.compileAndValidate Validate.emptyRegistry (Common.formatArraySchema "duration")) (mkDurations n)
        ]
    , Bench.bgroup
        "string"
        [ scaling "pattern" $ \n ->
            Bench.nf (Common.compileAndValidate Validate.emptyRegistry Common.patternArraySchema) (mkPatternStrings n)
        , scaling "minLength + maxLength" $ \n ->
            Bench.nf (Common.compileAndValidate Validate.emptyRegistry Common.stringLengthSchema) (mkLengthStrings n)
        ]
    , Bench.bgroup
        "numeric"
        [ scaling "multipleOf" $ \n ->
            Bench.nf (Common.compileAndValidate Validate.emptyRegistry Common.multipleOfSchema) (mkNumbers n)
        , scaling "bounds" $ \n ->
            Bench.nf (Common.compileAndValidate Validate.emptyRegistry Common.numericBoundsSchema) (mkNumbers n)
        ]
    , Bench.bgroup
        "enum-const"
        [ scaling "enum" $ \n ->
            Bench.nf (Common.compileAndValidate Validate.emptyRegistry enumSchema) (mkEnumValues n)
        , scaling "enum-reject" $ \n ->
            Bench.nf (Common.compileAndValidate Validate.emptyRegistry enumSchema) (mkEnumValuesMixed n)
        , scaling "const" $ \n ->
            Bench.nf (Common.compileAndValidate Validate.emptyRegistry constSchema) (mkConstValues n)
        , scaling "const-reject" $ \n ->
            Bench.nf (Common.compileAndValidate Validate.emptyRegistry constSchema) (mkConstValuesMixed n)
        ]
    , Bench.bgroup
        "errors"
        [ scaling "wrong type" $ \n ->
            Bench.nf (Common.compileAndValidate Validate.emptyRegistry Common.numberArraySchema) (mkStrings n)
        , scaling "allOf all fail" $ \n ->
            Bench.nf (Common.compileAndValidate Validate.emptyRegistry allOfSchema) (mkNegativeNumbers n)
        , scaling "wrong properties" $ \n ->
            Bench.nf (Common.compileAndValidate Validate.emptyRegistry (mkPropertiesSchema n)) (mkStringObject n)
        , scaling "missing required" $ \n ->
            Bench.nf (Common.compileAndValidate Validate.emptyRegistry (mkPropertiesSchema n)) Common.emptyObject
        , scaling "oneOf none match" $ \n ->
            Bench.nf (Common.compileAndValidate Validate.emptyRegistry (mkOneOfSchema n)) (mkNegativeNumbers n)
        ]
    , Bench.bgroup
        "registry"
        [ Bench.bench "compile+validate/ref-resolution"
            $ Bench.nf
              (Common.compileAndValidate registryFixture registryConsumerSchema)
              registryConsumerInput
        , scaling "cross-resource-pointer-refs" $ \n ->
            Bench.nf (Common.compileAndValidate (pointerTargetRegistry n) (mkPointerConsumerSchema n)) (mkNumericObject n)
        ]
    , Bench.bgroup
        "compile-vs-amortised"
        [ Bench.bench "cold/registry-1000-element-array"
            $ Bench.nf
              (Common.compileAndValidate registryFixture compileVsAmortisedSchema)
              compileVsAmortisedInput
        , Bench.bench "amortised/registry-1000-element-array"
            $ Bench.nf
              (Validate.validate compiledRegistryConsumer)
              compileVsAmortisedInput
        , Bench.bench "cold/simple-schema"
            $ Bench.nf
              (Common.compileAndValidate Validate.emptyRegistry Common.recordArraySchema)
              compileVsAmortisedRecords
        , Bench.bench "amortised/simple-schema"
            $ Bench.nf
              (Validate.validate compiledRecordsSchema)
              compileVsAmortisedRecords
        ]
    , Bench.bgroup
        "dynamic-anchor-heavy"
        [ Bench.bench "cold/dynamic-anchor-1000-element-array"
            $ Bench.nf
              (Common.compileAndValidate Common.dynamicAnchorRegistry Common.dynamicAnchorConsumerSchema)
              dynamicAnchorHeavyInput
        , Bench.bench "amortised/dynamic-anchor-1000-element-array"
            $ Bench.nf
              (Validate.validate compiledDynamicAnchorConsumer)
              dynamicAnchorHeavyInput
        ]
    , Bench.bgroup
        "decode"
        [ Bench.bench "schema-200-properties"
            $ Bench.whnf (Aeson.eitherDecode @Schema.Schema) decodableSchemaJson
        ]
    ]

scaling :: String -> (Int -> Bench.Benchmarkable) -> Bench.Benchmark
scaling name f = Bench.bgroup name (fmap (\n -> Bench.bench (show n) (f n)) scales)

scales :: [Int]
scales = [100, 1000, 10000]

itemsSchema :: Schema.Schema
itemsSchema =
  Schema.ObjectSchema
    Schema.emptySchema
      { Schema.schemaType = Just (NEL.singleton Schema.TypeArray)
      , Schema.schemaItems =
          Just
            ( Schema.ObjectSchema
                Schema.emptySchema
                  { Schema.schemaType = Just (NEL.singleton Schema.TypeNumber)
                  , Schema.schemaMinimum = Just 0
                  , Schema.schemaMaximum = Just 10000
                  }
            )
      , Schema.schemaMinItems = Just 1
      }

mkNumbers :: Int -> Aeson.Value
mkNumbers n =
  Aeson.Array
    . Vector.fromList
    . fmap (Aeson.Number . fromIntegral)
    $ [1 :: Int .. n]

uniqueItemsSchema :: Schema.Schema
uniqueItemsSchema =
  Schema.ObjectSchema
    Schema.emptySchema
      { Schema.schemaType = Just (NEL.singleton Schema.TypeArray)
      , Schema.schemaItems = Just Common.numberItemSchema
      , Schema.schemaUniqueItems = Just True
      }

mkNumbersAlmostUnique :: Int -> Aeson.Value
mkNumbersAlmostUnique n =
  Aeson.Array
    . Vector.fromList
    . fmap (Aeson.Number . fromIntegral)
    $ case n of
      0 -> []
      _ -> [1 :: Int .. n - 1] <> [1]

mkPrefixItemsSchema :: Int -> Schema.Schema
mkPrefixItemsSchema n =
  let
    prefixCount = max 1 (n `div` 2)
    prefixSchemas = mkNumberItem 0 NEL.:| fmap mkNumberItem [1 .. prefixCount - 1]
   in
    Schema.ObjectSchema
      Schema.emptySchema
        { Schema.schemaType = Just (NEL.singleton Schema.TypeArray)
        , Schema.schemaPrefixItems = Just prefixSchemas
        , Schema.schemaItems = Just Common.numberItemSchema
        }

mkNumberItem :: Int -> Schema.Schema
mkNumberItem i =
  Schema.ObjectSchema
    Schema.emptySchema
      { Schema.schemaType = Just (NEL.singleton Schema.TypeNumber)
      , Schema.schemaMinimum = Just (fromIntegral i)
      }

mkContainsSchema :: Int -> Schema.Schema
mkContainsSchema n =
  Schema.ObjectSchema
    Schema.emptySchema
      { Schema.schemaType = Just (NEL.singleton Schema.TypeArray)
      , Schema.schemaContains =
          Just
            ( Schema.ObjectSchema
                Schema.emptySchema
                  { Schema.schemaType = Just (NEL.singleton Schema.TypeNumber)
                  , Schema.schemaMinimum = Just (fromIntegral n - 1)
                  }
            )
      , Schema.schemaItems = Just Common.numberItemSchema
      }

mkPropertiesSchema :: Int -> Schema.Schema
mkPropertiesSchema n =
  let
    props =
      fmap
        ( \i ->
            ( fieldName i
            , Schema.ObjectSchema
                Schema.emptySchema{Schema.schemaType = Just (NEL.singleton Schema.TypeNumber), Schema.schemaMinimum = Just 0}
            )
        )
        [1 :: Int .. n]
   in
    Schema.ObjectSchema
      Schema.emptySchema
        { Schema.schemaType = Just (NEL.singleton Schema.TypeObject)
        , Schema.schemaProperties = Just (OMap.fromList props)
        , Schema.schemaRequired = Just (Set.fromList (fmap fst props))
        , Schema.schemaAdditionalProperties = Just (Schema.BoolSchema False)
        }

fieldName :: Int -> Text.Text
fieldName i = Common.txt ("field_" <> show i)

mkNumericObject :: Int -> Aeson.Value
mkNumericObject n =
  Aeson.Object
    . KeyMap.fromList
    . fmap (\i -> (Key.fromText (fieldName i), Aeson.Number (fromIntegral i)))
    $ [1 :: Int .. n]

mkPatternPropObjectArray :: Int -> Aeson.Value
mkPatternPropObjectArray n =
  let
    element =
      Aeson.Object
        . KeyMap.fromList
        . fmap (\i -> (Key.fromText (Common.txt ("p" <> show i <> "_1")), Aeson.Number (fromIntegral i)))
        $ [1 :: Int .. 8]
   in
    Aeson.Array . Vector.fromList . replicate n $ element

mkAdditionalPropsSchema :: Int -> Schema.Schema
mkAdditionalPropsSchema n =
  let
    knownProps =
      fmap
        ( \i ->
            ( Common.txt ("field_" <> show i)
            , Schema.ObjectSchema Schema.emptySchema{Schema.schemaType = Just (NEL.singleton Schema.TypeNumber)}
            )
        )
        [1 :: Int .. n `div` 2]
   in
    Schema.ObjectSchema
      Schema.emptySchema
        { Schema.schemaType = Just (NEL.singleton Schema.TypeObject)
        , Schema.schemaProperties = Just (OMap.fromList knownProps)
        , Schema.schemaAdditionalProperties =
            Just
              ( Schema.ObjectSchema
                  Schema.emptySchema
                    { Schema.schemaType = Just (NEL.singleton Schema.TypeNumber)
                    , Schema.schemaMinimum = Just 0
                    }
              )
        }

mkDependentSchemasSchema :: Int -> Schema.Schema
mkDependentSchemasSchema _n =
  Schema.ObjectSchema
    Schema.emptySchema
      { Schema.schemaType = Just (NEL.singleton Schema.TypeObject)
      , Schema.schemaDependentSchemas =
          Just
            ( OMap.fromList
                ( fmap
                    ( \i ->
                        ( Common.txt ("field_" <> show i)
                        , Schema.ObjectSchema
                            Schema.emptySchema
                              { Schema.schemaProperties =
                                  Just
                                    ( OMap.fromList
                                        [
                                          ( Common.txt ("field_" <> show (i + 1 :: Int))
                                          , Schema.ObjectSchema Schema.emptySchema{Schema.schemaType = Just (NEL.singleton Schema.TypeNumber)}
                                          )
                                        ]
                                    )
                              }
                        )
                    )
                    [1 :: Int .. 10]
                )
            )
      }

mkNestedSchema :: Int -> Schema.Schema
mkNestedSchema depth =
  foldr
    ( \i inner ->
        Schema.ObjectSchema
          Schema.emptySchema
            { Schema.schemaType = Just (NEL.singleton Schema.TypeObject)
            , Schema.schemaProperties = Just (OMap.fromList [(Common.txt ("k" <> show i), inner)])
            , Schema.schemaRequired = Just (Set.singleton (Common.txt ("k" <> show i)))
            }
    )
    (Schema.ObjectSchema Schema.emptySchema{Schema.schemaType = Just (NEL.singleton Schema.TypeString)})
    [1 :: Int .. depth]

mkNestedValue :: Int -> Aeson.Value
mkNestedValue depth =
  foldr
    (\i v -> Aeson.Object . KeyMap.singleton (Key.fromText (Common.txt ("k" <> show i))) $ v)
    (Aeson.String (Common.txt "leaf"))
    [1 :: Int .. depth]

mkRecords :: Int -> Aeson.Value
mkRecords n =
  Aeson.Array
    . Vector.fromList
    . fmap Common.mkRecord
    $ [1 :: Int .. n]

mkUnevaluatedPropsSchema :: Int -> Schema.Schema
mkUnevaluatedPropsSchema n =
  let
    props = fmap (\i -> (Common.txt ("field_" <> show i), Common.numberItemSchema)) [1 :: Int .. n `div` 2]
   in
    Schema.ObjectSchema
      Schema.emptySchema
        { Schema.schemaAllOf =
            Just (NEL.singleton (Schema.ObjectSchema Schema.emptySchema{Schema.schemaProperties = Just (OMap.fromList props)}))
        , Schema.schemaUnevaluatedProperties = Just Common.numberItemSchema
        }

unevaluatedItemsSchema :: Schema.Schema
unevaluatedItemsSchema =
  Schema.ObjectSchema
    Schema.emptySchema
      { Schema.schemaAllOf =
          Just (NEL.singleton (Schema.ObjectSchema Schema.emptySchema{Schema.schemaContains = Just Common.numberItemSchema}))
      , Schema.schemaUnevaluatedItems = Just Common.numberItemSchema
      }

allOfSchema :: Schema.Schema
allOfSchema =
  let
    constraints =
      fmap
        ( \i ->
            Schema.ObjectSchema
              Schema.emptySchema
                { Schema.schemaItems =
                    Just
                      (Schema.ObjectSchema Schema.emptySchema{Schema.schemaMultipleOf = Schema.mkPositiveScientific (fromIntegral (i :: Int))})
                }
        )
        (1 NEL.:| [2 .. 10])
   in
    Schema.ObjectSchema
      Schema.emptySchema
        { Schema.schemaType = Just (NEL.singleton Schema.TypeArray)
        , Schema.schemaAllOf = Just constraints
        , Schema.schemaItems =
            Just
              ( Schema.ObjectSchema
                  Schema.emptySchema{Schema.schemaType = Just (NEL.singleton Schema.TypeNumber), Schema.schemaMinimum = Just 0}
              )
        }

mkAnyOfSchema :: Int -> Schema.Schema
mkAnyOfSchema n =
  let
    branchCount = 10 :: Int
    rangeSize = max 1 (n `div` branchCount)
    branches =
      fmap
        ( \i ->
            Schema.ObjectSchema
              Schema.emptySchema
                { Schema.schemaItems =
                    Just
                      ( Schema.ObjectSchema
                          Schema.emptySchema
                            { Schema.schemaType = Just (NEL.singleton Schema.TypeNumber)
                            , Schema.schemaMinimum = Just (fromIntegral (i * rangeSize))
                            , Schema.schemaMaximum = Just (fromIntegral (i * rangeSize + rangeSize - 1))
                            }
                      )
                }
        )
        (0 NEL.:| [1 .. branchCount - 1])
   in
    Schema.ObjectSchema
      Schema.emptySchema
        { Schema.schemaType = Just (NEL.singleton Schema.TypeArray)
        , Schema.schemaAnyOf = Just branches
        }

mkOneOfSchema :: Int -> Schema.Schema
mkOneOfSchema n =
  let
    branchCount = 10 :: Int
    rangeSize = max 1 (n `div` branchCount)
    branches =
      fmap
        ( \i ->
            Schema.ObjectSchema
              Schema.emptySchema
                { Schema.schemaType = Just (NEL.singleton Schema.TypeNumber)
                , Schema.schemaMinimum = Just (fromIntegral (i * rangeSize))
                , Schema.schemaMaximum = Just (fromIntegral (i * rangeSize + rangeSize - 1))
                }
        )
        (0 NEL.:| [1 .. branchCount - 1])
   in
    Schema.ObjectSchema
      Schema.emptySchema
        { Schema.schemaType = Just (NEL.singleton Schema.TypeArray)
        , Schema.schemaItems = Just (Schema.ObjectSchema Schema.emptySchema{Schema.schemaOneOf = Just branches})
        }

nestedAllOfSchema :: Schema.Schema
nestedAllOfSchema =
  let
    layer constraints = Schema.ObjectSchema Schema.emptySchema{Schema.schemaAllOf = Just constraints}
    leaf i =
      Schema.ObjectSchema
        Schema.emptySchema
          { Schema.schemaItems =
              Just
                (Schema.ObjectSchema Schema.emptySchema{Schema.schemaMultipleOf = Schema.mkPositiveScientific (fromIntegral (i :: Int))})
          }
    range1to4 = 1 NEL.:| [2 .. 4]
    level1 = fmap leaf range1to4
    level2 = fmap (\i -> layer (fmap (\j -> leaf (i * 4 + j)) range1to4)) (range1to4 :: NEL.NonEmpty Int)
    level3 = layer level1 NEL.:| [layer level2]
   in
    Schema.ObjectSchema
      Schema.emptySchema
        { Schema.schemaType = Just (NEL.singleton Schema.TypeArray)
        , Schema.schemaAllOf = Just level3
        , Schema.schemaItems =
            Just
              ( Schema.ObjectSchema
                  Schema.emptySchema{Schema.schemaType = Just (NEL.singleton Schema.TypeNumber), Schema.schemaMinimum = Just 0}
              )
        }

mkDateTimes :: Int -> Aeson.Value
mkDateTimes n =
  Aeson.Array
    . Vector.fromList
    . fmap
      ( \i ->
          Aeson.String
            (Common.txt ("2024-" <> Common.pad (mod (i - 1) 12 + 1) <> "-" <> Common.pad (mod (i - 1) 28 + 1) <> "T12:00:00Z"))
      )
    $ [1 :: Int .. n]

mkDates :: Int -> Aeson.Value
mkDates n =
  Aeson.Array
    . Vector.fromList
    . fmap
      (\i -> Aeson.String (Common.txt ("2024-" <> Common.pad (mod (i - 1) 12 + 1) <> "-" <> Common.pad (mod (i - 1) 28 + 1))))
    $ [1 :: Int .. n]

mkTimes :: Int -> Aeson.Value
mkTimes n =
  Aeson.Array
    . Vector.fromList
    . fmap
      ( \i ->
          Aeson.String (Common.txt (Common.pad (mod i 24) <> ":" <> Common.pad (mod i 60) <> ":" <> Common.pad (mod i 60) <> "Z"))
      )
    $ [1 :: Int .. n]

mkEmails :: Int -> Aeson.Value
mkEmails n =
  Aeson.Array
    . Vector.fromList
    . fmap (\i -> Aeson.String (Common.txt ("user" <> show i <> "@example.com")))
    $ [1 :: Int .. n]

mkUris :: Int -> Aeson.Value
mkUris n =
  Aeson.Array
    . Vector.fromList
    . fmap (\i -> Aeson.String (Common.txt ("https://example.com/path/" <> show i <> "?q=test")))
    $ [1 :: Int .. n]

mkIpv4s :: Int -> Aeson.Value
mkIpv4s n =
  Aeson.Array
    . Vector.fromList
    . fmap
      ( \i ->
          Aeson.String
            ( Common.txt
                (show (mod i 256) <> "." <> show (mod (i * 7) 256) <> "." <> show (mod (i * 13) 256) <> "." <> show (mod (i * 31) 256))
            )
      )
    $ [1 :: Int .. n]

mkIpv6s :: Int -> Aeson.Value
mkIpv6s n =
  Aeson.Array
    . Vector.fromList
    . fmap (\i -> Aeson.String (Common.txt ("2001:db8::" <> show i)))
    $ [1 :: Int .. n]

mkUuids :: Int -> Aeson.Value
mkUuids n =
  Aeson.Array
    . Vector.fromList
    . fmap (\i -> Aeson.String (Common.txt ("550e8400-e29b-41d4-a716-" <> Common.padN 12 i)))
    $ [1 :: Int .. n]

mkHostnames :: Int -> Aeson.Value
mkHostnames n =
  Aeson.Array
    . Vector.fromList
    . fmap (\i -> Aeson.String (Common.txt ("host" <> show i <> ".example.com")))
    $ [1 :: Int .. n]

mkDurations :: Int -> Aeson.Value
mkDurations n =
  Aeson.Array
    . Vector.fromList
    . fmap
      ( \i ->
          Aeson.String
            ( Common.txt
                ( "P"
                    <> show (mod i 10 + 1)
                    <> "Y"
                    <> show (mod i 12 + 1)
                    <> "M"
                    <> show (mod i 28 + 1)
                    <> "DT"
                    <> show (mod i 24)
                    <> "H"
                    <> show (mod i 60)
                    <> "M"
                    <> show (mod i 60)
                    <> "S"
                )
            )
      )
    $ [1 :: Int .. n]

mkPatternStrings :: Int -> Aeson.Value
mkPatternStrings n =
  Aeson.Array
    . Vector.fromList
    . fmap (\i -> Aeson.String (Common.txt ("abc-" <> show i <> "-xyz")))
    $ [1 :: Int .. n]

mkLengthStrings :: Int -> Aeson.Value
mkLengthStrings n =
  Aeson.Array
    . Vector.fromList
    . fmap (\i -> Aeson.String (Common.txt (replicate (mod i 50 + 1) 'a')))
    $ [1 :: Int .. n]

enumSchema :: Schema.Schema
enumSchema =
  let
    vals = fmap (\i -> Aeson.String (Common.txt ("value_" <> show i))) [0 .. 49 :: Int]
   in
    Schema.ObjectSchema
      Schema.emptySchema
        { Schema.schemaType = Just (NEL.singleton Schema.TypeArray)
        , Schema.schemaItems = Just (Schema.ObjectSchema Schema.emptySchema{Schema.schemaEnum = Just vals})
        }

mkEnumValues :: Int -> Aeson.Value
mkEnumValues n =
  Aeson.Array
    . Vector.fromList
    . fmap (\i -> Aeson.String (Common.txt ("value_" <> show (mod i 50))))
    $ [1 :: Int .. n]

mkEnumValuesMixed :: Int -> Aeson.Value
mkEnumValuesMixed n =
  Aeson.Array
    . Vector.fromList
    . fmap
      ( \i ->
          Aeson.String
            ( Common.txt
                ( if even i
                    then "value_" <> show (mod i 50)
                    else "not_a_value_" <> show i
                )
            )
      )
    $ [1 :: Int .. n]

constSchema :: Schema.Schema
constSchema =
  Schema.ObjectSchema
    Schema.emptySchema
      { Schema.schemaType = Just (NEL.singleton Schema.TypeArray)
      , Schema.schemaItems = Just (Schema.ObjectSchema Schema.emptySchema{Schema.schemaConst = Just (Aeson.Number 42)})
      }

mkConstValues :: Int -> Aeson.Value
mkConstValues n =
  Aeson.Array
    . Vector.fromList
    . replicate n
    $ Aeson.Number 42

mkConstValuesMixed :: Int -> Aeson.Value
mkConstValuesMixed n =
  Aeson.Array
    . Vector.fromList
    . fmap (\i -> if even i then Aeson.Number 42 else Aeson.Number (fromIntegral i))
    $ [1 :: Int .. n]

mkStrings :: Int -> Aeson.Value
mkStrings n =
  Aeson.Array
    . Vector.fromList
    . fmap (\i -> Aeson.String (Common.txt ("string_" <> show i)))
    $ [1 :: Int .. n]

mkNegativeNumbers :: Int -> Aeson.Value
mkNegativeNumbers n =
  Aeson.Array
    . Vector.fromList
    . fmap (Aeson.Number . negate . fromIntegral)
    $ [1 :: Int .. n]

mkStringObject :: Int -> Aeson.Value
mkStringObject n =
  Aeson.Object
    . KeyMap.fromList
    . fmap (\i -> (Key.fromText (Common.txt ("field_" <> show i)), Aeson.String (Common.txt "wrong")))
    $ [1 :: Int .. n]

registryFixture :: Validate.Registry
registryFixture =
  foldr
    Validate.addSchema
    Validate.emptyRegistry
    [ namedSchema "https://example.test/positive"
        $ Schema.emptySchema
          { Schema.schemaType = Just (NEL.singleton Schema.TypeNumber)
          , Schema.schemaMinimum = Just 0
          }
    , namedSchema "https://example.test/short-string"
        $ Schema.emptySchema
          { Schema.schemaType = Just (NEL.singleton Schema.TypeString)
          , Schema.schemaMinLength = Just 1
          , Schema.schemaMaxLength = Just 32
          }
    , namedSchema "https://example.test/record"
        $ Schema.emptySchema
          { Schema.schemaType = Just (NEL.singleton Schema.TypeObject)
          , Schema.schemaRequired = Just (Set.fromList (fmap Common.txt ["id", "name"]))
          , Schema.schemaProperties =
              Just
                ( OMap.fromList
                    [
                      ( Common.txt "id"
                      , refSchema "https://example.test/positive"
                      )
                    ,
                      ( Common.txt "name"
                      , refSchema "https://example.test/short-string"
                      )
                    ]
                )
          }
    ]

namedSchema :: Text.Text -> Schema.SchemaObject -> Schema.Schema
namedSchema idVal obj =
  Schema.ObjectSchema obj{Schema.schemaId = Just (Schema.SchemaId idVal)}

refSchema :: Text.Text -> Schema.Schema
refSchema target =
  Schema.ObjectSchema Schema.emptySchema{Schema.schemaRef = Just (Schema.SchemaRef target)}

registryConsumerSchema :: Schema.Schema
registryConsumerSchema =
  Schema.ObjectSchema
    Schema.emptySchema
      { Schema.schemaType = Just (NEL.singleton Schema.TypeArray)
      , Schema.schemaItems = Just (refSchema "https://example.test/record")
      , Schema.schemaMinItems = Just 1
      }

registryConsumerInput :: Aeson.Value
registryConsumerInput =
  Aeson.Array
    . Vector.fromList
    . fmap
      ( \i ->
          Aeson.Object . KeyMap.fromList
            $ [ (Key.fromText (Common.txt "id"), Aeson.Number (fromIntegral i))
              , (Key.fromText (Common.txt "name"), Aeson.String (Common.txt ("item_" <> show i)))
              ]
      )
    $ [1 :: Int .. 200]

pointerTargetRegistry :: Int -> Validate.Registry
pointerTargetRegistry n = Validate.insertResource pointerTargetUri (mkPropertiesSchema n) Validate.emptyRegistry

pointerTargetUri :: Text.Text
pointerTargetUri = Common.txt "https://example.test/fields"

mkPointerConsumerSchema :: Int -> Schema.Schema
mkPointerConsumerSchema n =
  let
    props = fmap (\i -> (fieldName i, refSchema (pointerTargetUri <> "#/properties/" <> fieldName i))) [1 :: Int .. n]
   in
    Schema.ObjectSchema Schema.emptySchema{Schema.schemaProperties = Just (OMap.fromList props)}

compileVsAmortisedSchema :: Schema.Schema
compileVsAmortisedSchema =
  Schema.ObjectSchema
    Schema.emptySchema
      { Schema.schemaType = Just (NEL.singleton Schema.TypeArray)
      , Schema.schemaItems = Just (refSchema "https://example.test/record")
      , Schema.schemaMinItems = Just 1
      }

compileVsAmortisedInput :: Aeson.Value
compileVsAmortisedInput =
  Aeson.Array
    . Vector.fromList
    . fmap
      ( \i ->
          Aeson.Object . KeyMap.fromList
            $ [ (Key.fromText (Common.txt "id"), Aeson.Number (fromIntegral i))
              , (Key.fromText (Common.txt "name"), Aeson.String (Common.txt ("item_" <> show i)))
              ]
      )
    $ [1 :: Int .. 1000]

compiledRegistryConsumer :: Validate.Validator
compiledRegistryConsumer = Validate.compile registryFixture compileVsAmortisedSchema

compileVsAmortisedRecords :: Aeson.Value
compileVsAmortisedRecords =
  Aeson.Array
    . Vector.fromList
    . fmap Common.mkRecord
    $ [1 :: Int .. 1000]

compiledRecordsSchema :: Validate.Validator
compiledRecordsSchema = Validate.compile Validate.emptyRegistry Common.recordArraySchema

dynamicAnchorHeavyInput :: Aeson.Value
dynamicAnchorHeavyInput = Common.mkDynamicAnchorInput dynamicAnchorHeavyN

dynamicAnchorHeavyN :: Int
dynamicAnchorHeavyN = 1000

compiledDynamicAnchorConsumer :: Validate.Validator
compiledDynamicAnchorConsumer =
  Validate.compile Common.dynamicAnchorRegistry Common.dynamicAnchorConsumerSchema

decodableSchemaJson :: LBS.ByteString
decodableSchemaJson = Aeson.encode decodableSchema

decodableSchema :: Schema.Schema
decodableSchema =
  let
    props =
      fmap
        ( \i ->
            ( Common.txt ("field_" <> show i)
            , Schema.ObjectSchema
                Schema.emptySchema
                  { Schema.schemaType = Just (NEL.singleton Schema.TypeNumber)
                  , Schema.schemaMinimum = Just 0
                  , Schema.schemaMaximum = Just 1000
                  }
            )
        )
        [1 :: Int .. 200]
   in
    Schema.ObjectSchema
      Schema.emptySchema
        { Schema.schemaType = Just (NEL.singleton Schema.TypeObject)
        , Schema.schemaProperties = Just (OMap.fromList props)
        , Schema.schemaRequired = Just (Set.fromList (fmap fst (take 50 props)))
        , Schema.schemaAdditionalProperties = Just (Schema.BoolSchema False)
        }
