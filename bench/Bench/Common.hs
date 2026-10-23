module Bench.Common
  ( compileAndValidate
  , txt
  , pad
  , padN
  , emptyObject
  , mkRecord
  , numberItemSchema
  , numberArraySchema
  , formatArraySchema
  , recordSchema
  , recordArraySchema
  , patternPropsSchema
  , patternPropsArraySchema
  , propertyNamesSchema
  , patternArraySchema
  , stringLengthSchema
  , multipleOfSchema
  , numericBoundsSchema
  , notArraySchema
  , conditionalArraySchema
  , dynamicAnchorBaseSchema
  , dynamicAnchorConsumerSchema
  , dynamicAnchorRegistry
  , mkDynamicAnchorInput
  ) where

import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.List.NonEmpty qualified as NEL
import Data.Map.Ordered qualified as OMap
import Data.Set.Ordered qualified as OSet
import Data.Text qualified as Text
import Data.Vector qualified as Vector
import JsonSchema qualified as Schema

import JsonSchema.Validate qualified as Validate

compileAndValidate :: Validate.Registry -> Schema.Schema -> Aeson.Value -> [Validate.ValidationError]
compileAndValidate registry schema value =
  let
    validator = Validate.compile registry schema
   in
    Validate.validate validator value
{-# NOINLINE compileAndValidate #-}

txt :: String -> Text.Text
txt = Text.pack

pad :: Int -> String
pad n
  | n < 10 = '0' : show n
  | otherwise = show n

padN :: Int -> Int -> String
padN width n =
  let
    s = show n
   in
    replicate (max 0 (width - length s)) '0' <> s

emptyObject :: Aeson.Value
emptyObject = Aeson.Object KeyMap.empty

mkRecord :: Int -> Aeson.Value
mkRecord i =
  Aeson.Object
    . KeyMap.fromList
    $ [ (Key.fromText (txt "id"), Aeson.Number (fromIntegral i))
      , (Key.fromText (txt "name"), Aeson.String (txt ("item_" <> show i)))
      , (Key.fromText (txt "active"), Aeson.Bool True)
      , (Key.fromText (txt "score"), Aeson.Number (fromIntegral (i * 10)))
      ,
        ( Key.fromText (txt "tags")
        , Aeson.Array (Vector.fromList (fmap (Aeson.String . txt . ("tag" <>) . show) [1 :: Int .. 5]))
        )
      ]

numberItemSchema :: Schema.Schema
numberItemSchema =
  Schema.ObjectSchema Schema.emptySchema{Schema.schemaType = Just (NEL.singleton Schema.TypeNumber)}

numberArraySchema :: Schema.Schema
numberArraySchema =
  Schema.ObjectSchema
    Schema.emptySchema
      { Schema.schemaType = Just (NEL.singleton Schema.TypeArray)
      , Schema.schemaItems = Just numberItemSchema
      }

formatArraySchema :: String -> Schema.Schema
formatArraySchema fmt =
  Schema.ObjectSchema
    Schema.emptySchema
      { Schema.schemaType = Just (NEL.singleton Schema.TypeArray)
      , Schema.schemaItems =
          Just
            ( Schema.ObjectSchema
                Schema.emptySchema
                  { Schema.schemaType = Just (NEL.singleton Schema.TypeString)
                  , Schema.schemaFormat = Just (txt fmt)
                  }
            )
      }

recordSchema :: Schema.Schema
recordSchema =
  Schema.ObjectSchema
    Schema.emptySchema
      { Schema.schemaType = Just (NEL.singleton Schema.TypeObject)
      , Schema.schemaRequired = Just (OSet.fromList (fmap txt ["id", "name", "active", "score", "tags"]))
      , Schema.schemaProperties =
          Just
            ( OMap.fromList
                [
                  ( txt "id"
                  , Schema.ObjectSchema
                      Schema.emptySchema{Schema.schemaType = Just (NEL.singleton Schema.TypeNumber), Schema.schemaMinimum = Just 0}
                  )
                ,
                  ( txt "name"
                  , Schema.ObjectSchema
                      Schema.emptySchema{Schema.schemaType = Just (NEL.singleton Schema.TypeString), Schema.schemaMinLength = Just 1}
                  )
                , (txt "active", Schema.ObjectSchema Schema.emptySchema{Schema.schemaType = Just (NEL.singleton Schema.TypeBoolean)})
                ,
                  ( txt "score"
                  , Schema.ObjectSchema
                      Schema.emptySchema
                        { Schema.schemaType = Just (NEL.singleton Schema.TypeNumber)
                        , Schema.schemaMinimum = Just 0
                        , Schema.schemaMaximum = Just 1000
                        }
                  )
                ,
                  ( txt "tags"
                  , Schema.ObjectSchema
                      Schema.emptySchema
                        { Schema.schemaType = Just (NEL.singleton Schema.TypeArray)
                        , Schema.schemaItems =
                            Just (Schema.ObjectSchema Schema.emptySchema{Schema.schemaType = Just (NEL.singleton Schema.TypeString)})
                        , Schema.schemaMinItems = Just 1
                        }
                  )
                ]
            )
      , Schema.schemaAdditionalProperties = Just (Schema.BoolSchema False)
      }

recordArraySchema :: Schema.Schema
recordArraySchema =
  Schema.ObjectSchema
    Schema.emptySchema
      { Schema.schemaType = Just (NEL.singleton Schema.TypeArray)
      , Schema.schemaItems = Just recordSchema
      , Schema.schemaMinItems = Just 1
      }

patternPropsSchema :: Schema.Schema
patternPropsSchema =
  Schema.ObjectSchema
    Schema.emptySchema
      { Schema.schemaType = Just (NEL.singleton Schema.TypeObject)
      , Schema.schemaPatternProperties =
          Just
            ( OMap.fromList
                [
                  ( txt "^field_[0-9]+$"
                  , Schema.ObjectSchema
                      Schema.emptySchema{Schema.schemaType = Just (NEL.singleton Schema.TypeNumber), Schema.schemaMinimum = Just 0}
                  )
                ]
            )
      , Schema.schemaAdditionalProperties = Just (Schema.BoolSchema False)
      }

patternPropsArraySchema :: Schema.Schema
patternPropsArraySchema =
  let
    patternEntry i =
      ( txt ("^p" <> show (i :: Int) <> "_[0-9]+$")
      , Schema.ObjectSchema
          Schema.emptySchema{Schema.schemaType = Just (NEL.singleton Schema.TypeNumber)}
      )
    itemSchema =
      Schema.ObjectSchema
        Schema.emptySchema
          { Schema.schemaType = Just (NEL.singleton Schema.TypeObject)
          , Schema.schemaPatternProperties = Just (OMap.fromList (fmap patternEntry [1 .. 8]))
          , Schema.schemaAdditionalProperties = Just (Schema.BoolSchema False)
          }
   in
    Schema.ObjectSchema
      Schema.emptySchema
        { Schema.schemaType = Just (NEL.singleton Schema.TypeArray)
        , Schema.schemaItems = Just itemSchema
        }

propertyNamesSchema :: Schema.Schema
propertyNamesSchema =
  Schema.ObjectSchema
    Schema.emptySchema
      { Schema.schemaType = Just (NEL.singleton Schema.TypeObject)
      , Schema.schemaPropertyNames =
          Just
            ( Schema.ObjectSchema
                Schema.emptySchema
                  { Schema.schemaType = Just (NEL.singleton Schema.TypeString)
                  , Schema.schemaPattern = Just (txt "^field_[0-9]+$")
                  }
            )
      }

patternArraySchema :: Schema.Schema
patternArraySchema =
  Schema.ObjectSchema
    Schema.emptySchema
      { Schema.schemaType = Just (NEL.singleton Schema.TypeArray)
      , Schema.schemaItems =
          Just
            ( Schema.ObjectSchema
                Schema.emptySchema
                  { Schema.schemaType = Just (NEL.singleton Schema.TypeString)
                  , Schema.schemaPattern = Just (txt "^[a-z]+-[0-9]+-[a-z]+$")
                  }
            )
      }

stringLengthSchema :: Schema.Schema
stringLengthSchema =
  Schema.ObjectSchema
    Schema.emptySchema
      { Schema.schemaType = Just (NEL.singleton Schema.TypeArray)
      , Schema.schemaItems =
          Just
            ( Schema.ObjectSchema
                Schema.emptySchema
                  { Schema.schemaType = Just (NEL.singleton Schema.TypeString)
                  , Schema.schemaMinLength = Just 1
                  , Schema.schemaMaxLength = Just 100
                  }
            )
      }

multipleOfSchema :: Schema.Schema
multipleOfSchema =
  Schema.ObjectSchema
    Schema.emptySchema
      { Schema.schemaType = Just (NEL.singleton Schema.TypeArray)
      , Schema.schemaItems =
          Just
            ( Schema.ObjectSchema
                Schema.emptySchema
                  { Schema.schemaType = Just (NEL.singleton Schema.TypeNumber)
                  , Schema.schemaMultipleOf = Schema.mkPositiveScientific 7
                  }
            )
      }

numericBoundsSchema :: Schema.Schema
numericBoundsSchema =
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
                  , Schema.schemaExclusiveMinimum = Just (-1)
                  , Schema.schemaExclusiveMaximum = Just 10001
                  }
            )
      }

notArraySchema :: Schema.Schema
notArraySchema =
  Schema.ObjectSchema
    Schema.emptySchema
      { Schema.schemaType = Just (NEL.singleton Schema.TypeArray)
      , Schema.schemaItems =
          Just
            ( Schema.ObjectSchema
                Schema.emptySchema
                  { Schema.schemaNot =
                      Just (Schema.ObjectSchema Schema.emptySchema{Schema.schemaType = Just (NEL.singleton Schema.TypeString)})
                  , Schema.schemaAllOf =
                      Just
                        ( Schema.ObjectSchema Schema.emptySchema{Schema.schemaType = Just (NEL.singleton Schema.TypeNumber)}
                            NEL.:| [ Schema.ObjectSchema Schema.emptySchema{Schema.schemaMinimum = Just 0}
                                   , Schema.ObjectSchema Schema.emptySchema{Schema.schemaMaximum = Just 10000}
                                   ]
                        )
                  }
            )
      }

conditionalArraySchema :: Schema.Schema
conditionalArraySchema =
  Schema.ObjectSchema
    Schema.emptySchema
      { Schema.schemaType = Just (NEL.singleton Schema.TypeArray)
      , Schema.schemaItems =
          Just
            ( Schema.ObjectSchema
                Schema.emptySchema
                  { Schema.schemaIf =
                      Just
                        ( Schema.ObjectSchema
                            Schema.emptySchema
                              { Schema.schemaType = Just (NEL.singleton Schema.TypeNumber)
                              , Schema.schemaMinimum = Just 100
                              }
                        )
                  , Schema.schemaThen =
                      Just (Schema.ObjectSchema Schema.emptySchema{Schema.schemaMaximum = Just 10000})
                  , Schema.schemaElse =
                      Just
                        ( Schema.ObjectSchema
                            Schema.emptySchema
                              { Schema.schemaType = Just (NEL.singleton Schema.TypeNumber)
                              , Schema.schemaMinimum = Just 0
                              }
                        )
                  }
            )
      }

dynamicAnchorBaseSchema :: Schema.Schema
dynamicAnchorBaseSchema =
  Schema.ObjectSchema
    Schema.emptySchema
      { Schema.schemaId = Just (Schema.SchemaId (txt "https://example.test/dyn/base"))
      , Schema.schemaDynamicAnchor = Just (Schema.DynamicAnchor (txt "T"))
      , Schema.schemaType = Just (NEL.singleton Schema.TypeObject)
      , Schema.schemaRequired = Just (OSet.fromList (fmap txt ["id", "name"]))
      , Schema.schemaProperties =
          Just
            ( OMap.fromList
                [
                  ( txt "id"
                  , Schema.ObjectSchema
                      Schema.emptySchema
                        { Schema.schemaType = Just (NEL.singleton Schema.TypeNumber)
                        , Schema.schemaMinimum = Just 0
                        }
                  )
                ,
                  ( txt "name"
                  , Schema.ObjectSchema
                      Schema.emptySchema
                        { Schema.schemaType = Just (NEL.singleton Schema.TypeString)
                        , Schema.schemaMinLength = Just 1
                        , Schema.schemaMaxLength = Just 64
                        }
                  )
                ]
            )
      , Schema.schemaAdditionalProperties = Just (Schema.BoolSchema False)
      }

dynamicAnchorConsumerSchema :: Schema.Schema
dynamicAnchorConsumerSchema =
  Schema.ObjectSchema
    Schema.emptySchema
      { Schema.schemaId = Just (Schema.SchemaId (txt "https://example.test/dyn/consumer"))
      , Schema.schemaType = Just (NEL.singleton Schema.TypeArray)
      , Schema.schemaItems =
          Just
            ( Schema.ObjectSchema
                Schema.emptySchema
                  { Schema.schemaDynamicRef =
                      Just (Schema.DynamicRef (txt "https://example.test/dyn/base#T"))
                  }
            )
      , Schema.schemaMinItems = Just 1
      }

dynamicAnchorRegistry :: Validate.Registry
dynamicAnchorRegistry =
  foldr
    Validate.addSchema
    Validate.emptyRegistry
    [ dynamicAnchorBaseSchema
    , dynamicAnchorConsumerSchema
    ]

mkDynamicAnchorInput :: Int -> Aeson.Value
mkDynamicAnchorInput n =
  Aeson.Array
    . Vector.fromList
    . fmap
      ( \i ->
          Aeson.Object . KeyMap.fromList
            $ [ (Key.fromText (txt "id"), Aeson.Number (fromIntegral i))
              , (Key.fromText (txt "name"), Aeson.String (txt ("item_" <> show i)))
              ]
      )
    $ [1 :: Int .. n]
