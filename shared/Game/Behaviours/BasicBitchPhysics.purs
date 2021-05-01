module Pure.Behaviours.BasicBitchPhysics where

import Prelude
import Data.Exists (Exists, mkExists)
import Pure.Behaviour as B
import Pure.Entity (EntityBehaviour(..))
import Pure.Math (scalePoint)
import Pure.Types (EntityCommand(..), GameEvent)

init :: forall entity. Exists (EntityBehaviour EntityCommand GameEvent entity)
init =
  mkExists
    $ EntityBehaviour
        { state: unit
        , handleCommand:
            \command _ -> case command of
              Tick -> do
                B.updateEntity
                  ( \e@{ location, velocity, friction } ->
                      e { location = location + velocity, velocity = scalePoint friction velocity }
                  )
              _ -> pure unit
        }
