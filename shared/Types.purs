module Blatus.Types where

import Blatus.Entities.Behaviours.Farmable (CollectableSpawned)
import Blatus.Entities.Behaviours.ProvidesResource (ResourceProvided)
import Blatus.Entities (EntityClass)
import Data.Symbol (SProxy(..))
import Data.Variant (Variant, inj)
import Sisy.BuiltIn.Extensions.Bullets as Bullets
import Sisy.BuiltIn.Extensions.Collider (CollisionInfo)
import Sisy.Math (Point, Rect, origin)
import Sisy.Runtime.Entity (EntityId, Entity)
import Sisy.Runtime.Entity as Entity
import Sisy.Types (Empty)
import Sisy.BuiltIn (Damage, Impact, Mass(..), EntityDestroyed, BulletFired)

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
  = ( damage :: Damage
    , impact :: Impact
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

emptyEntity :: EntityId -> EntityClass -> Entity EntityCommand GameEvent GameEntity
emptyEntity id clss =
  Entity.emptyEntity id
    { networkSync: false
    , class: clss
    , health: 1.0
    , shield: 0.0
    , velocity: origin
    , mass: Fixed 1.0
    , friction: 1.0
    , aabb: { x: 0.0, y: 0.0, width: 0.0, height: 0.0 }
    }
