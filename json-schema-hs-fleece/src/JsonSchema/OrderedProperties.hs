module JsonSchema.OrderedProperties (OrderedProperties (..)) where

import Data.Aeson qualified as Aeson
import Data.Aeson.Encoding qualified as Encoding
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Bifunctor qualified as Bifunctor
import Data.Map.Ordered qualified as OMap
import Data.Monoid qualified as Monoid
import Data.Text qualified as Text
import Fleece.Aeson qualified as FA
import Fleece.Core qualified as FC

class OrderedProperties f where
  orderedProperties :: FC.Schema f a -> FC.Schema f (OMap.OMap Text.Text a)

instance OrderedProperties FA.Encoder where
  orderedProperties schema =
    let
      FA.Encoder encode = FC.schemaInterpreter schema
     in
      FC.Schema
        (FC.annotateName (FC.schemaName schema) "ordered properties")
        ( FA.Encoder
            ( Encoding.pairs
                . foldMap (\(key, value) -> Encoding.pair (Key.fromText key) (encode value))
                . OMap.assocs
            )
        )

instance OrderedProperties FA.ToValue where
  orderedProperties schema =
    let
      FA.ToValue toValue = FC.schemaInterpreter schema
     in
      FC.Schema
        (FC.annotateName (FC.schemaName schema) "ordered properties")
        ( FA.ToValue
            ( Aeson.Object
                . KeyMap.fromList
                . fmap (Bifunctor.bimap Key.fromText toValue)
                . OMap.assocs
            )
        )

instance OrderedProperties FA.Decoder where
  orderedProperties schema =
    let
      FA.Decoder decode = FC.schemaInterpreter schema
      annName = FC.annotateName (FC.schemaName schema) "ordered properties"
      parse k v = do
        decoded <- decode v
        pure [(Key.toText k, decoded)]
     in
      FC.Schema
        annName
        ( FA.Decoder
            ( Aeson.withObject
                (FC.nameToString annName)
                (fmap OMap.fromList . Monoid.getAp . KeyMap.foldMapWithKey (\k -> Monoid.Ap . parse k))
            )
        )
