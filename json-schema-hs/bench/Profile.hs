module Main (main) where

import Control.DeepSeq qualified as DeepSeq
import Control.Monad qualified as Monad
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.List.NonEmpty qualified as NEL
import Data.Map.Ordered qualified as OMap
import Data.Set.Ordered qualified as OSet
import Data.Text qualified as Text
import Data.Vector qualified as Vector

import Bench.Common qualified as Common
import JsonSchema qualified as Schema
import JsonSchema.Validate qualified as Validate

main :: IO ()
main = do
  let
    runN n f = Monad.replicateM_ n $ DeepSeq.force f `seq` pure ()

  putStrLn "array/items"
  runN 100 (Validate.validateOn largeArraySchema largeArray)

  putStrLn "array/uniqueItems"
  runN 100 (Validate.validateOn uniqueItemsSchema uniqueArray)

  putStrLn "array/contains"
  runN 100 (Validate.validateOn containsSchema largeArray)

  putStrLn "object/properties"
  runN 100 (Validate.validateOn wideSchema wideObject)

  putStrLn "object/patternProperties"
  runN 500 (Validate.validateOn Common.patternPropsSchema patternObject)

  putStrLn "object/propertyNames"
  runN 500 (Validate.validateOn Common.propertyNamesSchema wideObject)

  putStrLn "object/deep"
  runN 100 (Validate.validateOn deepSchema deepValue)

  putStrLn "object/records"
  runN 100 (Validate.validateOn Common.recordArraySchema recordArray)

  putStrLn "composition/allOf"
  runN 100 (Validate.validateOn allOfArraySchema numericArray)

  putStrLn "composition/anyOf"
  runN 100 (Validate.validateOn anyOfArraySchema numericArray)

  putStrLn "composition/oneOf"
  runN 100 (Validate.validateOn oneOfFanoutSchema oneOfArray)

  putStrLn "composition/not"
  runN 100 (Validate.validateOn Common.notArraySchema largeArray)

  putStrLn "composition/if-then-else"
  runN 100 (Validate.validateOn Common.conditionalArraySchema largeArray)

  putStrLn "composition/deepAllOf"
  runN 100 (Validate.validateOn deepAllOfSchema numericArray)

  putStrLn "format/date-time"
  runN 100 (Validate.validateOn (Common.formatArraySchema "date-time") dateTimeArray)

  putStrLn "format/email"
  runN 100 (Validate.validateOn (Common.formatArraySchema "email") emailArray)

  putStrLn "format/uri"
  runN 100 (Validate.validateOn (Common.formatArraySchema "uri") uriArray)

  putStrLn "format/ipv4"
  runN 100 (Validate.validateOn (Common.formatArraySchema "ipv4") ipv4Array)

  putStrLn "format/ipv6"
  runN 100 (Validate.validateOn (Common.formatArraySchema "ipv6") ipv6Array)

  putStrLn "format/hostname"
  runN 100 (Validate.validateOn (Common.formatArraySchema "hostname") hostnameArray)

  putStrLn "format/uuid"
  runN 100 (Validate.validateOn (Common.formatArraySchema "uuid") uuidArray)

  putStrLn "format/duration"
  runN 100 (Validate.validateOn (Common.formatArraySchema "duration") durationArray)

  putStrLn "string/pattern"
  runN 100 (Validate.validateOn Common.patternArraySchema patternStringsArray)

  putStrLn "string/length"
  runN 100 (Validate.validateOn Common.stringLengthSchema stringArray)

  putStrLn "numeric/multipleOf"
  runN 100 (Validate.validateOn Common.multipleOfSchema largeArray)

  putStrLn "numeric/bounds"
  runN 100 (Validate.validateOn Common.numericBoundsSchema largeArray)

  putStrLn "enum"
  runN 100 (Validate.validateOn largeEnumSchema enumTestArray)

  putStrLn "const"
  runN 100 (Validate.validateOn constArraySchema constTestArray)

  putStrLn "errors/wrong-type"
  runN 100 (Validate.validateOn Common.numberArraySchema allStringsArray)

  putStrLn "errors/allOf-all-fail"
  runN 100 (Validate.validateOn allOfArraySchema allNegativeArray)

  putStrLn "errors/wrong-properties"
  runN 100 (Validate.validateOn wideSchema wideStringObject)

  putStrLn "errors/missing-required"
  runN 100 (Validate.validateOn wideSchema Common.emptyObject)

  putStrLn "errors/oneOf-none-match"
  runN 100 (Validate.validateOn oneOfFanoutSchema allNegativeSmallArray)

  putStrLn "schema-path/deep-nesting-valid"
  runN 100 (Validate.validateOn deepSchema deepValue)

  putStrLn "schema-path/deep-allOf-valid"
  runN 100 (Validate.validateOn deepAllOfSchema numericArray)

  putStrLn "schema-path/nested-records-valid"
  runN 100 (Validate.validateOn Common.recordArraySchema recordArray)

  putStrLn "schema-path/deep-nesting-error"
  runN 100 (Validate.validateOn deepSchema deepValueBadLeaf)

  putStrLn "compile-once/amortised"
  let
    compiledRecords = Validate.compile Validate.emptyRegistry Common.recordArraySchema
  runN 100 (Validate.validate compiledRecords recordArray)

  putStrLn "dynamic-anchor-heavy/amortised"
  let
    compiledDynamic =
      Validate.compile Common.dynamicAnchorRegistry Common.dynamicAnchorConsumerSchema
    dynamicInput = Common.mkDynamicAnchorInput 1000
  runN 100 (Validate.validate compiledDynamic dynamicInput)

  putStrLn "Done."

largeArraySchema :: Schema.Schema
largeArraySchema =
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

largeArray :: Aeson.Value
largeArray = numberArray 5000

numberArray :: Int -> Aeson.Value
numberArray n =
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

uniqueArray :: Aeson.Value
uniqueArray = numberArray 500

containsSchema :: Schema.Schema
containsSchema =
  Schema.ObjectSchema
    Schema.emptySchema
      { Schema.schemaType = Just (NEL.singleton Schema.TypeArray)
      , Schema.schemaContains =
          Just
            ( Schema.ObjectSchema
                Schema.emptySchema
                  { Schema.schemaType = Just (NEL.singleton Schema.TypeNumber)
                  , Schema.schemaMinimum = Just 4999
                  }
            )
      , Schema.schemaItems = Just Common.numberItemSchema
      }

wideSchema :: Schema.Schema
wideSchema =
  let
    props =
      fmap
        ( \i ->
            ( Common.txt ("field_" <> show i)
            , Schema.ObjectSchema
                Schema.emptySchema{Schema.schemaType = Just (NEL.singleton Schema.TypeNumber), Schema.schemaMinimum = Just 0}
            )
        )
        [1 :: Int .. 500]
   in
    Schema.ObjectSchema
      Schema.emptySchema
        { Schema.schemaType = Just (NEL.singleton Schema.TypeObject)
        , Schema.schemaProperties = Just (OMap.fromList props)
        , Schema.schemaRequired = Just (OSet.fromList (fmap fst props))
        , Schema.schemaAdditionalProperties = Just (Schema.BoolSchema False)
        }

wideObject :: Aeson.Value
wideObject =
  Aeson.Object
    . KeyMap.fromList
    . fmap (\i -> (Key.fromText (Common.txt ("field_" <> show i)), Aeson.Number (fromIntegral i)))
    $ [1 :: Int .. 500]

patternObject :: Aeson.Value
patternObject =
  Aeson.Object
    . KeyMap.fromList
    . fmap (\i -> (Key.fromText (Common.txt ("field_" <> show i)), Aeson.Number (fromIntegral i)))
    $ [1 :: Int .. 200]

deepSchema :: Schema.Schema
deepSchema =
  foldr
    ( \i inner ->
        Schema.ObjectSchema
          Schema.emptySchema
            { Schema.schemaType = Just (NEL.singleton Schema.TypeObject)
            , Schema.schemaProperties = Just (OMap.fromList [(Common.txt ("k" <> show i), inner)])
            , Schema.schemaRequired = Just (OSet.singleton (Common.txt ("k" <> show i)))
            }
    )
    (Schema.ObjectSchema Schema.emptySchema{Schema.schemaType = Just (NEL.singleton Schema.TypeString)})
    [1 :: Int .. 1000]

deepValue :: Aeson.Value
deepValue =
  foldr
    (\i v -> Aeson.Object . KeyMap.singleton (Key.fromText (Common.txt ("k" <> show i))) $ v)
    (Aeson.String (Common.txt "leaf"))
    [1 :: Int .. 1000]

recordArray :: Aeson.Value
recordArray =
  Aeson.Array
    . Vector.fromList
    . fmap Common.mkRecord
    $ [1 :: Int .. 500]

allOfArraySchema :: Schema.Schema
allOfArraySchema =
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

numericArray :: Aeson.Value
numericArray = numberArray 500

anyOfArraySchema :: Schema.Schema
anyOfArraySchema =
  let
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
                            , Schema.schemaMinimum = Just (fromIntegral (i * 50 :: Int))
                            , Schema.schemaMaximum = Just (fromIntegral (i * 50 + 49 :: Int))
                            }
                      )
                }
        )
        (0 NEL.:| [1 .. 9 :: Int])
   in
    Schema.ObjectSchema
      Schema.emptySchema
        { Schema.schemaType = Just (NEL.singleton Schema.TypeArray)
        , Schema.schemaAnyOf = Just branches
        }

oneOfFanoutSchema :: Schema.Schema
oneOfFanoutSchema =
  let
    branches =
      fmap
        ( \i ->
            Schema.ObjectSchema
              Schema.emptySchema
                { Schema.schemaType = Just (NEL.singleton Schema.TypeNumber)
                , Schema.schemaMinimum = Just (fromIntegral (i * 100 :: Int))
                , Schema.schemaMaximum = Just (fromIntegral (i * 100 + 99 :: Int))
                }
        )
        (0 NEL.:| [1 .. 19])
   in
    Schema.ObjectSchema
      Schema.emptySchema
        { Schema.schemaType = Just (NEL.singleton Schema.TypeArray)
        , Schema.schemaItems = Just (Schema.ObjectSchema Schema.emptySchema{Schema.schemaOneOf = Just branches})
        }

oneOfArray :: Aeson.Value
oneOfArray = numberArray 200

deepAllOfSchema :: Schema.Schema
deepAllOfSchema =
  let
    layer cs = Schema.ObjectSchema Schema.emptySchema{Schema.schemaAllOf = Just cs}
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

dateTimeArray :: Aeson.Value
dateTimeArray =
  mkStringArray
    . fmap
      (\i -> Common.txt ("2024-" <> Common.pad (mod (i - 1) 12 + 1) <> "-" <> Common.pad (mod (i - 1) 28 + 1) <> "T12:00:00Z"))
    $ [1 :: Int .. 5000]

mkStringArray :: [Text.Text] -> Aeson.Value
mkStringArray = Aeson.Array . Vector.fromList . fmap Aeson.String

emailArray :: Aeson.Value
emailArray =
  mkStringArray
    . fmap (\i -> Common.txt ("user" <> show i <> "@example.com"))
    $ [1 :: Int .. 1000]

uriArray :: Aeson.Value
uriArray =
  mkStringArray
    . fmap (\i -> Common.txt ("https://example.com/path/" <> show i <> "?q=test"))
    $ [1 :: Int .. 1000]

ipv4Array :: Aeson.Value
ipv4Array =
  mkStringArray
    . fmap
      ( \i ->
          Common.txt
            (show (mod i 256) <> "." <> show (mod (i * 7) 256) <> "." <> show (mod (i * 13) 256) <> "." <> show (mod (i * 31) 256))
      )
    $ [1 :: Int .. 1000]

ipv6Array :: Aeson.Value
ipv6Array =
  mkStringArray
    . fmap (\i -> Common.txt ("2001:db8::" <> show i))
    $ [1 :: Int .. 1000]

hostnameArray :: Aeson.Value
hostnameArray =
  mkStringArray
    . fmap (\i -> Common.txt ("host" <> show i <> ".example.com"))
    $ [1 :: Int .. 1000]

uuidArray :: Aeson.Value
uuidArray =
  mkStringArray
    . fmap (\i -> Common.txt ("550e8400-e29b-41d4-a716-" <> Common.padN 12 i))
    $ [1 :: Int .. 5000]

durationArray :: Aeson.Value
durationArray =
  mkStringArray
    . fmap
      ( \i ->
          Common.txt
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
    $ [1 :: Int .. 5000]

patternStringsArray :: Aeson.Value
patternStringsArray =
  mkStringArray
    . fmap (\i -> Common.txt ("abc-" <> show i <> "-xyz"))
    $ [1 :: Int .. 1000]

stringArray :: Aeson.Value
stringArray =
  mkStringArray
    . fmap (\i -> Common.txt (replicate (mod i 50 + 1) 'a'))
    $ [1 :: Int .. 5000]

largeEnumSchema :: Schema.Schema
largeEnumSchema =
  let
    enumValues = fmap (\i -> Aeson.String (Common.txt ("value_" <> show i))) [0 .. 49 :: Int]
   in
    Schema.ObjectSchema
      Schema.emptySchema
        { Schema.schemaType = Just (NEL.singleton Schema.TypeArray)
        , Schema.schemaItems = Just (Schema.ObjectSchema Schema.emptySchema{Schema.schemaEnum = Just enumValues})
        }

enumTestArray :: Aeson.Value
enumTestArray =
  Aeson.Array
    . Vector.fromList
    . fmap (\i -> Aeson.String (Common.txt ("value_" <> show (mod i 50))))
    $ [1 :: Int .. 5000]

constArraySchema :: Schema.Schema
constArraySchema =
  Schema.ObjectSchema
    Schema.emptySchema
      { Schema.schemaType = Just (NEL.singleton Schema.TypeArray)
      , Schema.schemaItems = Just (Schema.ObjectSchema Schema.emptySchema{Schema.schemaConst = Just (Aeson.Number 42)})
      }

constTestArray :: Aeson.Value
constTestArray =
  Aeson.Array
    . Vector.fromList
    . replicate 5000
    $ Aeson.Number 42

allStringsArray :: Aeson.Value
allStringsArray =
  Aeson.Array
    . Vector.fromList
    . fmap (\i -> Aeson.String (Common.txt ("not_a_number_" <> show i)))
    $ [1 :: Int .. 5000]

allNegativeArray :: Aeson.Value
allNegativeArray =
  Aeson.Array
    . Vector.fromList
    . fmap (Aeson.Number . negate . fromIntegral)
    $ [1 :: Int .. 500]

wideStringObject :: Aeson.Value
wideStringObject =
  Aeson.Object
    . KeyMap.fromList
    . fmap (\i -> (Key.fromText (Common.txt ("field_" <> show i)), Aeson.String (Common.txt "wrong")))
    $ [1 :: Int .. 500]

allNegativeSmallArray :: Aeson.Value
allNegativeSmallArray =
  Aeson.Array
    . Vector.fromList
    . fmap (Aeson.Number . negate . fromIntegral)
    $ [1 :: Int .. 200]

deepValueBadLeaf :: Aeson.Value
deepValueBadLeaf =
  foldr
    (\i v -> Aeson.Object . KeyMap.singleton (Key.fromText (Common.txt ("k" <> show i))) $ v)
    (Aeson.Number 42)
    [1 :: Int .. 1000]
