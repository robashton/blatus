module Pure.Game.Entities.Classes where

import Prelude
import Data.Generic.Rep (class Generic)
import GenericJSON (writeTaggedSumRep, taggedSumRep)
import Data.Show.Generic (genericShow)
import Simple.JSON (class ReadForeign, class WriteForeign)

data EntityClass
  = Tank
  | Bullet

type GameEntity
  = ( | NetworkSync )

type NetworkSync
  = ( networkSync :: Boolean
    , class :: EntityClass
    )

derive instance genericEntityClass :: Generic EntityClass _

instance showEntityClass :: Show EntityClass where
  show = genericShow

instance writeForeignEntityClass :: WriteForeign EntityClass where
  writeImpl = writeTaggedSumRep

instance readForeignEntityClass :: ReadForeign EntityClass where
  readImpl = taggedSumRep
