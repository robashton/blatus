module Blatus.Entities.Turret where

import Blatus.Types (EntityCommand, GameEntity, GameEvent)
import Blatus.Entities (EntityClass(..), TurretArgs)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Sisy.Math (Point, centreRect, origin)
import Sisy.Runtime.Entity (Entity, EntityId, HtmlColor(..), sprite)
import Sisy.BuiltIn (Mass(..))

init :: EntityId -> Point -> TurretArgs -> Entity EntityCommand GameEvent GameEntity
init id location args =
  { id
  , location
  , velocity: origin
  , friction: 1.0
  , rotation: 0.0
  , mass: Infinite
  , health: 100.0
  , shield: 0.0
  , behaviour: Nil
  , class: Turret args
  , networkSync: false
  , aabb: centreRect location { x: 0.0, y: 0.0, width: 15.0, height: 15.0 }
  , renderables:
      ( sprite
          { transform = centreRect origin { x: 0.0, y: 0.0, width: 15.0, height: 15.0 }
          , color = HtmlColor "#ccc"
          , image = Just "turret"
          }
      )
        : Nil
  }
