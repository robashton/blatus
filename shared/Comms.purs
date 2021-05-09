module Blatus.Comms where

-- TODO: Move this into Sisy by getting rid of the specifics
import Prelude
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Variant (Variant)
import Sisy.Runtime.Entity (EntityId)
import Blatus.GenericJSON (writeTaggedSumRep, taggedSumRep)
import Sisy.Math (Point, Rect)
import Blatus.Types (EntityCommand, GameEvent, RegisteredPlayer)
import Blatus.Entities.Types (EntityClass)
import Simple.JSON (class ReadForeign, class WriteForeign)

data ServerMsg
  = Sync GameSync
  | PlayerAdded EntityId
  | PlayerRemoved EntityId
  | PlayerSync EntitySync
  | Welcome WelcomeInfo
  | ServerCommand { cmd :: Variant EntityCommand, id :: EntityId }
  | ServerEvents (Array (Variant GameEvent))
  | Pong Int

type PlayerListItem
  = { playerId :: String
    , score :: Int
    , lastTick :: Int
    }

data ClientMsg
  = ClientCommand (Variant EntityCommand)
  | Quit
  | Ping Int

type WelcomeInfo
  = { gameUrl :: String
    , playerId :: String
    }

type GameSync
  = { world :: Rect
    , entities :: Array EntitySync
    , tick :: Int
    , players :: Array RegisteredPlayer
    }

type EntitySync
  = { id :: EntityId
    , class :: EntityClass
    , location :: Point
    , velocity :: Point
    , rotation :: Number
    , health :: Number
    , shield :: Number
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
