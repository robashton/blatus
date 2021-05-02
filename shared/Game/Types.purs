module Pure.Types where

import Prelude
import Data.Maybe (Maybe)
import Data.Symbol (SProxy(..))
import Data.Variant (Variant, inj)
import Foreign (readString)
import Pure.Behaviours.Damageable (EntityDestroyed)
import Pure.Behaviours.FiresBullets (BulletFired)
import Pure.BuiltIn.Bullets as Bullets
import Pure.BuiltIn.Collider (CollisionInfo)
import Pure.Entity (EntityId)
import Pure.Math (Point)
import Pure.Runtime.Types (Empty)
import Simple.JSON (class ReadForeign, class WriteForeign, write)

type RegisteredPlayer
  = { id :: EntityId
    , lastTick :: Int
    , score :: Int
    }

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

type GameEvent
  = ( bulletFired :: BulletFired
    , entityCollided :: CollisionInfo
    , bulletHit :: Bullets.BulletHit
    , entityDestroyed :: EntityDestroyed
    , playerSpawn :: PlayerSpawn
    )

type PlayerSpawn
  = { id :: EntityId, x :: Number, y :: Number }

playerSpawn :: PlayerSpawn -> Variant GameEvent
playerSpawn = inj (SProxy :: SProxy "playerSpawn")
