module Pure.Comms where

-- TODO: Move this into Engine by getting rid of the specifics

import Prelude

import Data.Foldable (foldl)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List (toUnfoldable)
import Data.Map as Map
import GenericJSON (writeTaggedSumRep, taggedSumRep)
import Pure.Entities.Bullet as Bullet
import Pure.Entities.Tank as Tank
import Pure.Entity (Entity, EntityClass(..), EntityId)
import Pure.Runtime.Control (Game)
import Pure.Runtime.Control as Control
import Pure.Game.Main as Main
import Pure.Math (Point, Rect)
import Pure.Types (EntityCommand(..), GameEvent(..))
import Simple.JSON (class ReadForeign, class WriteForeign)

data ServerMsg = Sync GameSync
               | NewEntity EntitySync
               | PlayerSync EntitySync
               | Welcome WelcomeInfo
               | ServerCommand { cmd :: EntityCommand, id  :: EntityId }
               | ServerEvents (Array GameEvent)
               | UpdatePlayerList (Array PlayerListItem)
               | EntityDeleted EntityId
               | Pong Int

type PlayerListItem = { playerId :: String
                      , score :: Int
                      , lastTick :: Int
                      }

data ClientMsg = ClientCommand EntityCommand
               | Ping Int

type WelcomeInfo = { gameUrl :: String
                   , playerId :: String
                   }

type GameSync = { world :: Rect
                , entities :: Array EntitySync
                , tick :: Int
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

  
gameToSync :: forall cmd ev. Game cmd ev -> Int -> GameSync
gameToSync { entities, world } tick =
  { entities: toUnfoldable $ map entityToSync $ Map.values entities
  , world
  , tick}

entityToSync :: forall cmd ev. Entity cmd ev -> EntitySync
entityToSync { id, class: c, location, velocity, rotation } =
  { id, class: c, location, velocity, rotation }


gameFromSync :: GameSync -> Game EntityCommand GameEvent
gameFromSync { entities, world } = Main.init { world = world
                                             , entities = foldl (\m e -> Map.insert e.id (entityFromSync e) m) mempty entities
                                             }

mergeSyncInfo :: Game EntityCommand GameEvent -> GameSync -> Game EntityCommand GameEvent
mergeSyncInfo game sync =
  foldl (\acc es -> Control.discardEvents $ Control.sendCommand es.id (UpdateServerState { location: es.location
                                                                                         , velocity: es.velocity
                                                                                         , rotation: es.rotation 
                                                                                         }) acc
    ) game sync.entities


mergePlayerSync :: Game EntityCommand GameEvent -> EntitySync -> Game EntityCommand GameEvent
mergePlayerSync game es =
  Control.discardEvents $ Control.sendCommand es.id (UpdateServerState { location: es.location
                                                                 , velocity: es.velocity
                                                                 , rotation: es.rotation 
                                                                 }) game
                        

entityFromSync :: EntitySync -> Entity EntityCommand GameEvent
entityFromSync sync =
  let blank = case sync.class of
                Tank -> Tank.init sync.id Tank.Client sync.location
                Bullet -> Bullet.init sync.id sync.location sync.velocity
                Controller -> Bullet.init sync.id sync.location sync.velocity -- TODO: This just needs to die a death
   in
   blank { location = sync.location
         , velocity = sync.velocity
         , rotation = sync.rotation }
