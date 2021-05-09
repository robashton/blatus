module Blatus.Types where

import Blatus.Entities.Behaviours.Farmable (CollectableSpawned)
import Blatus.Entities.Behaviours.ProvidesResource (ResourceProvided)
import Blatus.Entities.Types (EntityClass)
import Data.Maybe (Maybe)
import Data.Symbol (SProxy(..))
import Data.Variant (Variant, inj)
import Sisy.BuiltIn.Behaviours.BasicBitchPhysics (Mass)
import Sisy.BuiltIn.Behaviours.Damageable (EntityDestroyed)
import Sisy.BuiltIn.Behaviours.FiresBullets (BulletFired)
import Sisy.BuiltIn.Extensions.Bullets as Bullets
import Sisy.BuiltIn.Extensions.Collider (CollisionInfo)
import Sisy.Math (Point, Rect)
import Sisy.Runtime.Entity (EntityId)
import Sisy.Types (Empty)

type RegisteredPlayer
  = { id :: EntityId
    , lastTick :: Int
    , score :: Int -- probably not anymore
    , availableRock :: Int
    }

type PlayerSpawn
  = { id :: EntityId, x :: Number, y :: Number }

playerSpawn :: forall r. PlayerSpawn -> Variant ( playerSpawn :: PlayerSpawn | r )
playerSpawn = inj (SProxy :: SProxy "playerSpawn")

impact :: forall r. { force :: Number, source :: EntityId } -> Variant ( impact :: { force :: Number, source :: EntityId } | r )
impact = inj (SProxy :: SProxy "impact")

type GameEntity
  = ( networkSync :: Boolean
    , class :: EntityClass
    , health :: Number
    , shield :: Number
    , velocity :: Point
    , mass :: Mass
    , friction :: Number
    , aabb :: Rect
    )

type EntityCommand
  = ( damage :: { amount :: Number, location :: Point, source :: Maybe EntityId }
    , impact :: { force :: Number, source :: EntityId }
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
    , resourceProvided :: ResourceProvided
    , collectableSpawned :: CollectableSpawned
    )
