module Pure.Comms where

import Prelude
import Pure.Game (Game)
import Pure.Game as Game
import Pure.Entity (EntityId(..), Entity, EntityClass(..), EntityCommand(..))
import Data.List (toUnfoldable)
import Data.Foldable (foldl)
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
import Pure.Math (Rect)

data ServerMsg = InitialState GameSync
               | ServerCommand { cmd :: EntityCommand, id  :: EntityId }
               | NewEntity EntitySync

data ClientMsg = ClientCommand EntityCommand


type GameSync = { world :: Rect
                , entities :: Array EntitySync
                , playerName :: String
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

derive instance genericClientMsg :: Generic ClientMsg _
instance showClientMsg :: Show ClientMsg where
  show = genericShow
instance writeForeignClientMsg :: WriteForeign ClientMsg where
  writeImpl = writeTaggedSumRep
instance readForeignClientMsg :: ReadForeign ClientMsg where
  readImpl = taggedSumRep

  
gameToSync :: String -> Game -> GameSync
gameToSync playerName { entities, world } =
  { entities: toUnfoldable $ map entityToSync $ Map.values entities
  , playerName
  , world }

entityToSync :: Entity -> EntitySync
entityToSync { id, class: c, location, velocity, rotation } =
  { id, class: c, location, velocity, rotation }


gameFromSync :: GameSync -> Game
gameFromSync { entities, world } = {
  world,
  entities: foldl (\m e -> Map.insert e.id (entityFromSync e) m) mempty entities
  }

entityFromSync :: EntitySync -> Entity
entityFromSync sync =
  let blank = case sync.class of
                Tank -> Game.tank sync.id sync.location
                Bullet -> Game.bullet sync.id sync.location sync.velocity
   in
   blank { location = sync.location
         , velocity = sync.velocity
         , rotation = sync.rotation }
