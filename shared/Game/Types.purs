module Pure.Types where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)
import Foreign (readString)
import GenericJSON (writeTaggedSumRep, taggedSumRep)
import Pure.BuiltIn.Bullets as Bullets
import Pure.Entity (EntityId)
import Pure.Math (Point)
import Simple.JSON (class ReadForeign, class WriteForeign, write)

type RegisteredPlayer
  = { id :: EntityId
    , lastTick :: Int
    , score :: Int
    }

newtype Empty
  = Empty Unit

empty :: Empty
empty = Empty unit

instance writeEmpty :: WriteForeign Empty where
  writeImpl _ = write ""

instance readEmpty :: ReadForeign Empty where
  readImpl f = empty <$ readString f

instance showEmpty :: Show Empty where
  show _ = "Empty"

instance eqEmpty :: Eq Empty where
  eq _ _ = true

type EntityCommand
  = ( damage :: { amount :: Number, source :: Maybe EntityId }
    , pushForward :: Empty
    , pushBackward :: Empty
    , turnLeft :: Empty
    , turnRight :: Empty
    , startFireBullet :: Empty
    , stopFireBullet :: Empty
    , stopPushForward :: Empty
    , stopPushBackward :: Empty
    , stopTurnLeft :: Empty
    , stopTurnRight :: Empty
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
