module JsonSchema.Validate.Dialect
  ( Dialect (..)
  , defaultDialect
  , dialectForMetaschema
  , dialectSupports
  , VocabularyFlags (..)
  , dialectVocabularyFlags
  , coreVocabularyUri
  , applicatorVocabularyUri
  , unevaluatedVocabularyUri
  , validationVocabularyUri
  , metaDataVocabularyUri
  , formatAnnotationVocabularyUri
  , contentVocabularyUri
  , formatAssertionVocabularyUri
  , standardVocabularies
  ) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text

import JsonSchema qualified as Schema
import JsonSchema.Validate.Registry qualified as Registry

data Dialect = Dialect
  { dialectAssertFormats :: Bool
  , dialectVocabularies :: Set.Set Text.Text
  }

defaultDialect :: Dialect
defaultDialect =
  Dialect
    { dialectAssertFormats = False
    , dialectVocabularies = standardVocabularies
    }

standardVocabularies :: Set.Set Text.Text
standardVocabularies =
  Set.fromList
    [ coreVocabularyUri
    , applicatorVocabularyUri
    , unevaluatedVocabularyUri
    , validationVocabularyUri
    , metaDataVocabularyUri
    , formatAnnotationVocabularyUri
    , contentVocabularyUri
    ]

coreVocabularyUri :: Text.Text
coreVocabularyUri = "https://json-schema.org/draft/2020-12/vocab/core"

applicatorVocabularyUri :: Text.Text
applicatorVocabularyUri = "https://json-schema.org/draft/2020-12/vocab/applicator"

unevaluatedVocabularyUri :: Text.Text
unevaluatedVocabularyUri = "https://json-schema.org/draft/2020-12/vocab/unevaluated"

validationVocabularyUri :: Text.Text
validationVocabularyUri = "https://json-schema.org/draft/2020-12/vocab/validation"

metaDataVocabularyUri :: Text.Text
metaDataVocabularyUri = "https://json-schema.org/draft/2020-12/vocab/meta-data"

formatAnnotationVocabularyUri :: Text.Text
formatAnnotationVocabularyUri = "https://json-schema.org/draft/2020-12/vocab/format-annotation"

contentVocabularyUri :: Text.Text
contentVocabularyUri = "https://json-schema.org/draft/2020-12/vocab/content"

dialectForMetaschema :: Registry.Registry -> Text.Text -> Dialect
dialectForMetaschema registry uri =
  let
    vocabs =
      maybe standardVocabularies (resolveVocabularies Set.empty registry) (Registry.resourceUri uri)
   in
    Dialect
      { dialectAssertFormats = Set.member formatAssertionVocabularyUri vocabs
      , dialectVocabularies = vocabs
      }

resolveVocabularies ::
  Set.Set Registry.ResourceKey -> Registry.Registry -> Registry.ResourceKey -> Set.Set Text.Text
resolveVocabularies visited registry key =
  case Registry.lookupResourceSchema key registry of
    Just (Schema.ObjectSchema obj)
      | Just vocab <- Schema.schemaVocabulary obj -> Map.keysSet vocab
      | Just parent <- Registry.resourceUri . Schema.metaschemaUriToText =<< Schema.schemaMetaschema obj
      , Set.notMember parent visitedWithKey ->
          resolveVocabularies visitedWithKey registry parent
    _ -> standardVocabularies
  where
    visitedWithKey = Set.insert key visited

formatAssertionVocabularyUri :: Text.Text
formatAssertionVocabularyUri =
  "https://json-schema.org/draft/2020-12/vocab/format-assertion"

dialectSupports :: Text.Text -> Dialect -> Bool
dialectSupports uri dialect = Set.member uri (dialectVocabularies dialect)

data VocabularyFlags = VocabularyFlags
  { vocabularyFlagsValidation :: !Bool
  , vocabularyFlagsApplicator :: !Bool
  , vocabularyFlagsUnevaluated :: !Bool
  }

dialectVocabularyFlags :: Dialect -> VocabularyFlags
dialectVocabularyFlags dialect =
  VocabularyFlags
    { vocabularyFlagsValidation = dialectSupports validationVocabularyUri dialect
    , vocabularyFlagsApplicator = dialectSupports applicatorVocabularyUri dialect
    , vocabularyFlagsUnevaluated = dialectSupports unevaluatedVocabularyUri dialect
    }
