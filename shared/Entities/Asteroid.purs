module Blatus.Entities.Asteroid where

import Blatus.Entities.Behaviours.Farmable as Farmable
import Blatus.Entities (CollectableType(..), EntityClass(..))
import Blatus.Types (EntityCommand, GameEvent, GameEntity)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Sisy.Math (Point, centreRect, origin)
import Sisy.BuiltIn (Mass(..))
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
  , behaviour: (Farmable.init { dropEvery: 50.0, drop: { width: 10.0, height: 10.0, lifetime: 600, collectableType: Rock 5 } }) : Nil
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
