module Blatus.BuildMenu where

import Prelude
import Blatus.Entities (EntityClass(..))
import Blatus.Types (RegisteredPlayer, BuildTemplate(..))
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Sisy.Math (Point, distance)
import Sisy.Runtime.Scene (Game, entityById)

type BuildAction cmd ev entity
  = { template :: BuildTemplate
    , info :: RegisteredPlayer -> Game cmd ev entity -> BuildActionInfo
    , get :: Point -> RegisteredPlayer -> Game cmd ev entity -> Maybe EntityClass
    }

type BuildActionInfo
  = { template :: BuildTemplate
    , available :: Boolean
    , description :: String
    }

actions :: forall cmd ev entity. List (BuildAction cmd ev entity)
actions = turret : Nil

turret ::
  forall cmd ev entity.
  BuildAction cmd ev entity
turret =
  { template: BuildTemplate "turret"
  , info:
      \p _game ->
        if p.availableRock > 50 then
          { template: BuildTemplate "turret"
          , available: true
          , description: "Standalone turret that'll fire at your enemies"
          }
        else
          { template: BuildTemplate "turret"
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
