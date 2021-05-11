module Blatus.Entities.Collectable where

import Blatus.Types (EntityCommand, GameEntity, GameEvent)
import Blatus.Entities (CollectableArgs, EntityClass(..))
import Blatus.Entities.Behaviours.ProvidesResource as ProvidesResource
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Sisy.BuiltIn.Behaviours.BasicBitchPhysics as BasicBitchPhysics
import Sisy.BuiltIn.Behaviours.DisappearsAfter as DisappearsAfter
import Sisy.Math (Point, centreRect, origin)
import Sisy.Runtime.Entity (Entity, EntityId, HtmlColor(..), sprite)
import Sisy.BuiltIn (Mass(..))

data EntityMode
  = Server
  | Client

init :: EntityId -> Point -> CollectableArgs -> Entity EntityCommand GameEvent GameEntity
init id location args@{ width, height, lifetime } =
  { id
  , location
  , velocity: origin
  , friction: 1.0
  , rotation: 0.0
  , mass: NoMass
  , health: 100.0
  , shield: 0.0
  , behaviour:
      BasicBitchPhysics.init
        : (DisappearsAfter.init lifetime)
        : (ProvidesResource.init args.collectableType)
        : Nil
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
