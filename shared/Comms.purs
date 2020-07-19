module Pure.Comms where

import Prelude
import Pure.Game (Game)
import Pure.Entity (EntityId(..), Entity, EntityClass(..))
import Data.List (toUnfoldable)
import Pure.Math (Point(..))
import Control.Alt ((<|>))
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Generic.Rep.Show (genericShow)
import Foreign (Foreign)
import Foreign as Foreign
import Data.Map as Map
import Simple.JSON (class ReadForeign, class WriteForeign, readImpl, writeImpl)
import GenericJSON (writeTaggedSumRep, taggedSumRep)

data ServerMsg = InitialState GameSync


type GameSync = {
  entities :: Array EntitySync
  }


type EntitySync = { id :: EntityId
                  , class ::  EntityClass
                  , location :: Point
                  , velocity :: Point
                  , rotation :: Number
                  }


derive instance genericServerMsg :: Generic ServerMsg _
instance showServerMsg :: Show ServerMsg where
  show = genericShow
instance writeForeignServerMsg :: WriteForeign ServerMsg where
  writeImpl = writeTaggedSumRep
instance readForeignServerMsg :: ReadForeign ServerMsg where
  readImpl = taggedSumRep

  
gameToSync :: Game -> GameSync
gameToSync { entities } =
  { entities: toUnfoldable $ map entityToSync $ Map.values entities }

entityToSync :: Entity -> EntitySync
entityToSync { id, class: c, location, velocity, rotation } =
  { id, class: c, location, velocity, rotation }


 
