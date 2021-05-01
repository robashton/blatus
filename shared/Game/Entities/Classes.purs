module Pure.Game.Entities.Classes where

import Prelude
import Data.Generic.Rep (class Generic)
import GenericJSON (writeTaggedSumRep, taggedSumRep)
import Data.Show.Generic (genericShow)
import Simple.JSON (class ReadForeign, class WriteForeign)

-- A lot of this can probably now go in shared behaviours
-- as we can have typeclasses around 'syncState' that tell us how to convert
-- an entity to and from network protocol
data EntityClass
  = Tank
  | Bullet

-- It is envisaged that some of these find there way into common components
type GameEntity
  = ( networkSync :: Boolean
    , class :: EntityClass
    , health :: Number
    , shield :: Number
    )

derive instance genericEntityClass :: Generic EntityClass _

instance showEntityClass :: Show EntityClass where
  show = genericShow

instance writeForeignEntityClass :: WriteForeign EntityClass where
  writeImpl = writeTaggedSumRep

instance readForeignEntityClass :: ReadForeign EntityClass where
  readImpl = taggedSumRep
