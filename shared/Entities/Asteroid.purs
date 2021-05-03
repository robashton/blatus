module Blatus.Entities.Asteroid where

import Prelude
import Data.List (List(..), (:))
import Sisy.BuiltIn.Behaviours.BasicBitchPhysics as BasicBitchPhysics
import Sisy.Runtime.Entity (Entity, EntityId, HtmlColor(..), sprite)
import Sisy.Math (Point)
import Blatus.Types (EntityCommand, GameEvent, GameEntity, EntityClass(..))

data EntityMode
  = Server
  | Client

init :: EntityId -> Point -> Number -> Number -> Entity EntityCommand GameEvent GameEntity
init id location width height =
  { id
  , location
  , width
  , height
  , velocity: { x: 0.0, y: 0.0 }
  , friction: 1.0
  , rotation: 0.0
  , mass: 200.0
  , health: 100.0
  , shield: 0.0
  , behaviour: BasicBitchPhysics.init : Nil
  , class: Asteroid
  , networkSync: true
  , renderables:
      ( sprite
          { transform =
            { x: -2.5
            , y: -2.5
            , width: 5.0
            , height: 5.0
            }
          , color = HtmlColor "#ccc"
          }
      )
        : Nil
  }
