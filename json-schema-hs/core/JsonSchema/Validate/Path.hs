module JsonSchema.Validate.Path
  ( InstancePath
  , PathSegment (..)
  , instanceSegment
  , instanceIndex
  , instancePathToList
  , instancePathDepth
  , SchemaPath
  , schemaSegment
  , schemaIndex
  , schemaPathToList
  ) where

import Data.Foldable qualified as Foldable
import Data.Sequence qualified as Seq
import Data.Text qualified as Text

newtype InstancePath = InstancePath (Seq.Seq PathSegment)
  deriving newtype (Monoid, Semigroup)

data PathSegment
  = PropSegment !Text.Text
  | IndexSegment !Int
  deriving stock (Eq, Show)

instanceSegment :: Text.Text -> InstancePath
instanceSegment seg = InstancePath (Seq.singleton (PropSegment seg))

instanceIndex :: Int -> InstancePath
instanceIndex i = InstancePath (Seq.singleton (IndexSegment i))

instancePathToList :: InstancePath -> [Text.Text]
instancePathToList (InstancePath segs) = fmap renderPathSegment (Foldable.toList segs)

renderPathSegment :: PathSegment -> Text.Text
renderPathSegment seg = case seg of
  PropSegment t -> t
  IndexSegment i -> Text.pack (show i)

instancePathDepth :: InstancePath -> Int
instancePathDepth (InstancePath segs) = Seq.length segs

newtype SchemaPath = SchemaPath (Seq.Seq Text.Text)
  deriving newtype (Monoid, Semigroup)

schemaSegment :: Text.Text -> SchemaPath
schemaSegment seg = SchemaPath (Seq.singleton seg)

schemaIndex :: Int -> SchemaPath
schemaIndex = schemaSegment . renderPathSegment . IndexSegment

schemaPathToList :: SchemaPath -> [Text.Text]
schemaPathToList (SchemaPath segs) = Foldable.toList segs
