module Pure.Entities.Bullet where

import Prelude
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Pure.Behaviours.BasicBitchPhysics as BasicBitchPhysics
import Pure.Entity (Entity, EntityClass(..), EntityId, HtmlColor(..), sprite)
import Pure.Math (Point, point)
import Pure.Types (EntityCommand, GameEvent)

data EntityMode
  = Server
  | Client

init :: EntityId -> Point -> Point -> Entity EntityCommand GameEvent
init id location velocity =
  { id
  , location: location
  , class: Bullet
  , width: 5.0
  , height: 5.0
  , velocity: point 0.0 0.0
  , friction: 1.0
  , rotation: 0.0
  , mass: 20.0
  , networkSync: false
  , health: 1.0
  , shield: 0.0
  , behaviour: BasicBitchPhysics.init : Nil
  , renderables:
      ( sprite
          { transform =
            { x: -2.5
            , y: -2.5
            , width: 5.0
            , height: 5.0
            }
          , color = HtmlColor "#ff0"
          }
      )
        : Nil
  }
