module Blatus.Types where

import Prelude
import Blatus.Entities (EntityClass)
import Blatus.Entities.Behaviours.Farmable (CollectableSpawned)
import Blatus.Entities.Behaviours.ProvidesResource (ResourceProvided)
import Data.Symbol (SProxy(..))
import Data.Variant (Variant, inj)
import Simple.JSON (class ReadForeign, class WriteForeign)
import Sisy.BuiltIn (Damage, Impact, Mass(..), EntityDestroyed, BulletFired)
import Sisy.BuiltIn.Extensions.Bullets as Bullets
import Sisy.BuiltIn.Extensions.Collider (CollisionInfo)
import Sisy.Math (Rect, Point, origin)
import Sisy.Runtime.Entity (EntityId, Entity)
import Sisy.Runtime.Entity as Entity
import Sisy.Types (Empty)

type RegisteredPlayer
  = { id :: EntityId
    , lastTick :: Int
    , score :: Int
    , availableRock :: Int
    }

type PlayerSpawn
  = { id :: EntityId, x :: Number, y :: Number }

playerSpawn :: forall r. PlayerSpawn -> Variant ( playerSpawn :: PlayerSpawn | r )
playerSpawn = inj (SProxy :: SProxy "playerSpawn")

newtype BuildTemplate
  = BuildTemplate String

derive instance eqBuildTemplate :: Eq BuildTemplate

derive instance ordBuildTemplate :: Ord BuildTemplate

derive newtype instance btReadForeign :: ReadForeign BuildTemplate

derive newtype instance btWriteForeign :: WriteForeign BuildTemplate

derive newtype instance btShow :: Show BuildTemplate

build :: forall r. Build -> Variant ( build :: Build | r )
build = inj (SProxy :: SProxy "build")

buildRequested :: forall r. BuildRequested -> Variant ( buildRequested :: BuildRequested | r )
buildRequested = inj (SProxy :: SProxy "buildRequested")

type Build
  = { location :: Point
    , template :: BuildTemplate
    }

type BuildRequested
  = { location :: Point
    , entity :: EntityId
    , template :: BuildTemplate
    }

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
    , build :: Build
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
    , build :: Build
    )

type GameEvent
  = ( bulletFired :: BulletFired
    , entityCollided :: CollisionInfo
    , bulletHit :: Bullets.BulletHit
    , entityDestroyed :: EntityDestroyed
    , playerSpawn :: PlayerSpawn
    , resourceProvided :: ResourceProvided
    , collectableSpawned :: CollectableSpawned
    , buildRequested :: BuildRequested
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
