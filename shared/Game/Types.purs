module Pure.Types where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Maybe (Maybe)
import GenericJSON (writeTaggedSumRep, taggedSumRep)
import Pure.BuiltIn.Bullets as Bullets
import Pure.Entity (EntityId)
import Pure.Math (Point)
import Simple.JSON (class ReadForeign, class WriteForeign)

type RegisteredPlayer
  = { id :: EntityId
    , lastTick :: Int
    , score :: Int
    }

type EntityCommand
  = ( damage :: { amount :: Number, source :: Maybe EntityId }
    , pushForward :: Unit
    , pushBackward :: Unit
    , turnLeft :: Unit
    , turnRight :: Unit
    , startFireBullet :: Unit
    , stopFireBullet :: Unit
    , stopPushForward :: Unit
    , stopPushBackward :: Unit
    , stopTurnLeft :: Unit
    , stopTurnRight :: Unit
    , updateServerState ::
        { location :: Point
        , velocity :: Point
        , rotation :: Number
        }
    )

data GameEvent
  = BulletFired { owner :: EntityId, location :: Point, velocity :: Point, power :: Number }
  | EntityCollided { left :: EntityId, right :: EntityId, force :: Number }
  | BulletHit Bullets.BulletHit
  | EntityDestroyed { entity :: EntityId, destroyer :: Maybe EntityId }
  | PlayerSpawn { id :: EntityId, x :: Number, y :: Number }

-- Probably Variant (..)
-- derive instance genericEntityCommand :: Generic EntityCommand _
-- 
-- instance showEntityCommand :: Show EntityCommand where
--   show = genericShow
-- 
-- instance writeForeignEntityCommand :: WriteForeign EntityCommand where
--   writeImpl = writeTaggedSumRep
-- 
-- instance readForeignEntityCommand :: ReadForeign EntityCommand where
--   readImpl = taggedSumRep
-- 
-- derive instance eqEntityCommand :: Eq EntityCommand
derive instance genericGameEvent :: Generic GameEvent _

instance showGameEvent :: Show GameEvent where
  show = genericShow

instance writeForeignGameEvent :: WriteForeign GameEvent where
  writeImpl = writeTaggedSumRep

instance readForeignGameEvent :: ReadForeign GameEvent where
  readImpl = taggedSumRep
