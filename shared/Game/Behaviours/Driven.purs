module Pure.Behaviours.Driven where

import Prelude
import Data.Exists (Exists, mkExists)
import Pure.Behaviour as B
import Pure.Behaviours.BasicBitchPhysics as BasicBitchPhysics
import Pure.Entity (EntityBehaviour(..))
import Pure.Types (EntityCommand(..), GameEvent)

type DrivenConfig
  = { maxSpeed :: Number
    , acceleration :: Number
    , turningSpeed :: Number
    }

init ::
  forall entity.
  DrivenConfig -> Exists (EntityBehaviour EntityCommand GameEvent (BasicBitchPhysics.Required entity))
init config =
  mkExists
    $ EntityBehaviour
        { state: { forward: false, backward: false, left: false, right: false }
        , handleCommand:
            \command s -> case command of
              Tick -> do
                ( if s.forward then
                    BasicBitchPhysics.applyThrust config.acceleration config.maxSpeed
                  else if s.backward then
                    BasicBitchPhysics.applyThrust (-config.acceleration) config.maxSpeed
                  else
                    pure unit
                )
                ( if s.left then
                    B.rotate (-config.turningSpeed)
                  else if s.right then
                    B.rotate config.turningSpeed
                  else
                    pure unit
                )
                pure s
              PushForward -> pure s { forward = true }
              PushBackward -> pure s { backward = true }
              TurnLeft -> pure s { left = true }
              TurnRight -> pure s { right = true }
              StopPushForward -> pure s { forward = false }
              StopPushBackward -> pure s { backward = false }
              StopTurnLeft -> pure s { left = false }
              StopTurnRight -> pure s { right = false }
              _ -> pure s
        }
