module Pure.Comms where

-- TODO: Move this into Engine by getting rid of the specifics
import Prelude
import Data.Foldable (foldl)
import Data.Generic.Rep (class Generic)
import Data.List (toUnfoldable)
import Data.Map as Map
import Data.Show.Generic (genericShow)
import Data.Variant (Variant)
import GenericJSON (writeTaggedSumRep, taggedSumRep)
import Pure.Entities.Bullet as Bullet
import Pure.Entities.Tank as Tank
import Pure.Entity (Entity, EntityId)
import Pure.Game.Entities.Classes (EntityClass)
import Pure.Math (Point, Rect)
import Pure.Runtime.Scene (Game)
import Pure.Runtime.Scene as Scene
import Pure.Types (EntityCommand(..), GameEvent(..), RegisteredPlayer)
import Simple.JSON (class ReadForeign, class WriteForeign)
import Unsafe.Coerce (unsafeCoerce)

data ServerMsg
  = Sync GameSync
  | PlayerAdded EntityId
  | PlayerRemoved EntityId
  | PlayerSync EntitySync
  | Welcome WelcomeInfo
  | ServerCommand { cmd :: Variant EntityCommand, id :: EntityId }
  | ServerEvents (Array GameEvent)
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
