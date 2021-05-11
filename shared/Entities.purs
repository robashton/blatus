module Blatus.Entities where

import Prelude
import Blatus.GenericJSON (writeTaggedSumRep, taggedSumRep)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Simple.JSON (class ReadForeign, class WriteForeign)

data EntityClass
  = Tank
  | Asteroid { width :: Number, height :: Number }
  | Collectable CollectableArgs

type CollectableArgs
  = { width :: Number
    , height :: Number
    , lifetime :: Int
    , collectableType :: CollectableType
    }

data CollectableType
  = Rock Int

derive instance genericEntityClass :: Generic EntityClass _

instance showEntityClass :: Show EntityClass where
  show = genericShow

derive instance eqEntityClass :: Eq EntityClass

instance writeForeignEntityClass :: WriteForeign EntityClass where
  writeImpl = writeTaggedSumRep

instance readForeignEntityClass :: ReadForeign EntityClass where
  readImpl = taggedSumRep

derive instance genericCollectableType :: Generic CollectableType _

instance showCollectableType :: Show CollectableType where
  show = genericShow

derive instance eqCollectableType :: Eq CollectableType

instance writeForeignCollectableType :: WriteForeign CollectableType where
  writeImpl = writeTaggedSumRep

instance readForeignCollectableType :: ReadForeign CollectableType where
  readImpl = taggedSumRep
