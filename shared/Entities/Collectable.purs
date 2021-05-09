module Blatus.Entities.Collectable where

import Prelude
import Blatus.Types (CollectableArgs, EntityClass(..), EntityCommand, GameEntity, GameEvent)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Sisy.BuiltIn.Behaviours.BasicBitchPhysics (Mass(..))
import Sisy.BuiltIn.Behaviours.BasicBitchPhysics as BasicBitchPhysics
import Sisy.Math (Point, centreRect, origin)
import Sisy.Runtime.Entity (Entity, EntityId, HtmlColor(..), sprite)

data EntityMode
  = Server
  | Client

init :: EntityId -> Point -> CollectableArgs -> Entity EntityCommand GameEvent GameEntity
init id location args@{ width, height } =
  { id
  , location
  , velocity: origin
  , friction: 0.0
  , rotation: 0.0
  , mass: NoMass
  , health: 100.0
  , shield: 0.0
  , behaviour: BasicBitchPhysics.init : Nil
  , class: Collectable args
  , networkSync: false
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
