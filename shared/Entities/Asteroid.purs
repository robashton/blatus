module Blatus.Entities.Asteroid where

import Prelude
import Blatus.Types (EntityCommand, GameEvent, GameEntity, EntityClass(..))
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Sisy.BuiltIn.Behaviours.BasicBitchPhysics (Mass(..))
import Sisy.Math (Point, centreRect, origin)
import Sisy.Runtime.Entity (Entity, EntityId, HtmlColor(..), sprite)

data EntityMode
  = Server
  | Client

init :: EntityId -> Point -> Number -> Number -> Entity EntityCommand GameEvent GameEntity
init id location width height =
  { id
  , location
  , velocity: { x: 0.0, y: 0.0 }
  , friction: 0.0
  , rotation: 0.0
  , mass: Infinite
  , health: 100.0
  , shield: 0.0
  , behaviour: Nil
  , class: Asteroid { width, height }
  , networkSync: true
  , aabb: centreRect location { x: 0.0, y: 0.0, width, height }
  , renderables:
      ( sprite
          { transform = centreRect origin { x: 0.0, y: 0.0, width, height }
          , color = HtmlColor "#ccc"
          , image = Just "asteroid"
          }
      )
        : Nil
  }
