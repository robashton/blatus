module Blatus.Entities.Tank where

import Prelude
import Blatus.Types (EntityClass(..), GameEntity, EntityCommand, GameEvent)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Sisy.BuiltIn.Behaviours.BasicBitchPhysics (Mass(..))
import Sisy.BuiltIn.Behaviours.BasicBitchPhysics as BasicBitchPhysics
import Sisy.BuiltIn.Behaviours.Damageable as Damageable
import Sisy.BuiltIn.Behaviours.Driven as Driven
import Sisy.BuiltIn.Behaviours.FiresBullets as FiresBullets
import Sisy.BuiltIn.Behaviours.NetworkSync as NetworkSync
import Sisy.BuiltIn.Behaviours.Regenerates as Regenerates
import Sisy.Math (Point, centreRect, origin)
import Sisy.Runtime.Entity (Entity, EntityId, sprite)

maxHealth :: Number
maxHealth = 100.0

maxShield :: Number
maxShield = 50.0

init :: EntityId -> Point -> Entity EntityCommand GameEvent GameEntity
init id location =
  { id
  , location
  , class: Tank
  , velocity: { x: 0.0, y: 0.0 }
  , friction: 0.98
  , rotation: (-0.25)
  , mass: Fixed 1000.0
  , health: maxHealth
  , shield: maxShield
  , networkSync: true
  , aabb: centreRect location { x: 0.0, y: 0.0, width: 25.0, height: 25.0 }
  , behaviour:
      Damageable.init
        : FiresBullets.init { max: 5, speed: 9.0, rate: 2, power: 25.0, coolOffPeriod: 5 }
        : Driven.init { maxSpeed: 5.0, acceleration: 200.0, turningSpeed: 0.015 }
        : NetworkSync.init { force: 0.08 }
        : Regenerates.init { maxHealth, maxShield, healthDelay: 0, healthRegen: 0.0, shieldDelay: 180, shieldRegen: 0.2 }
        : BasicBitchPhysics.init
        : Nil
  , renderables:
      ( sprite
          { transform = centreRect origin { x: 0.0, y: 0.0, width: 30.0, height: 30.0 }
          , image = Just "shield"
          , id = "shield"
          }
      )
        : ( sprite
              { transform = centreRect origin { x: 0.0, y: 0.0, width: 25.0, height: 25.0 }
              , image = Just "ship"
              , id = "ship"
              }
          )
        : Nil
  }
