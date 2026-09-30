{-# LANGUAGE TemplateHaskell #-}

module JsonSchema.Validate.Metaschema
  ( metaschemaDocuments
  , metaschemaRegistry
  , documentRegistry
  ) where

import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.Either qualified as Either
import Data.FileEmbed qualified as FileEmbed
import Data.List qualified as List

import JsonSchema qualified as Schema
import JsonSchema.Validate.Registry qualified as Registry

metaschemaDocuments :: [(FilePath, Either String Schema.Schema)]
metaschemaDocuments =
  fmap (fmap Aeson.eitherDecodeStrict)
    . filter (List.isSuffixOf ".json" . fst)
    $ embeddedFiles

embeddedFiles :: [(FilePath, BS.ByteString)]
embeddedFiles = $(FileEmbed.embedDir "data/metaschema/draft2020-12")

metaschemaRegistry :: Registry.Registry
metaschemaRegistry =
  foldr (Registry.overlay . Registry.buildRegistry) Registry.emptyRegistry (Either.rights (fmap snd metaschemaDocuments))

documentRegistry :: Registry.Registry -> Schema.Schema -> Registry.Registry
documentRegistry external schema =
  foldr Registry.overlay metaschemaRegistry [Registry.buildRegistry schema, external]
