module Blatus.BuildMenu where

import Prelude
import Blatus.Entities (EntityClass(..))
import Blatus.Entities.Turret as Turret
import Blatus.Types (BuildTemplate(..), EntityCommand, GameEvent, RegisteredPlayer, GameEntity)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Sisy.Math (Point, distance)
import Sisy.Runtime.Entity (Entity, EntityId(..))
import Sisy.Runtime.Scene (Game, entityById)

type BuildAction
  = { template :: BuildTemplate
    , info :: RegisteredPlayer -> Game EntityCommand GameEvent GameEntity -> BuildActionInfo
    , get :: Point -> RegisteredPlayer -> Game EntityCommand GameEvent GameEntity -> Maybe EntityClass
    }

type BuildActionInfo
  = { template :: BuildTemplate
    , available :: Boolean
    , description :: String
    , build :: EntityId -> Point -> Entity EntityCommand GameEvent GameEntity
    }

actions :: List BuildAction
actions = turret : Nil

turret :: BuildAction
turret =
  { template: BuildTemplate "turret"
  , info:
      \p _game ->
        if p.availableRock > 50 then
          { template: BuildTemplate "turret"
          , available: true
          , build: \id location -> Turret.init id location { owner: EntityId "" }
          , description: "Standalone turret that'll fire at your enemies"
          }
        else
          { template: BuildTemplate "turret"
          , build: \id location -> Turret.init id location { owner: EntityId "" }
          , available: false
          , description: "Requires 50 rock"
          }
  , get:
      \l p g ->
        entityById p.id g
          >>= (\e -> if (distance l e.location) > 200.0 then Nothing else Just e)
          >>= (\e -> if (distance l e.location) < 10.0 then Nothing else Just e)
          <#> (_.id)
          <#> (Turret <<< { owner: _ })
  }
