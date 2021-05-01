module Pure.Entities.Bullet where

import Prelude
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Pure.Behaviours.BasicBitchPhysics as BasicBitchPhysics
import Pure.Entity (Entity, EntityId, HtmlColor(..), sprite)
import Pure.Game.Entities.Classes (EntityClass(..), GameEntity)
import Pure.Math (Point)
import Pure.Types (EntityCommand, GameEvent)

data EntityMode
  = Server
  | Client

init :: EntityId -> Point -> Point -> Entity EntityCommand GameEvent GameEntity
init id location velocity =
  { id
  , location
  , width: 5.0
  , height: 5.0
  , velocity
  , friction: 1.0
  , rotation: 0.0
  , mass: 20.0
  , health: 1.0
  , shield: 0.0
  , behaviour: BasicBitchPhysics.init : Nil
  , class: Bullet
  , networkSync: false
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
