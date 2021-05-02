module Sisy.Runtime.Ticks where

import Prelude
import Data.Int as Int
import Data.Tuple (Tuple(..))

type State
  = { timeAtLastFrame :: Number
    , leftoverTime :: Number
    , timePerFrame :: Number
    }

init :: Number -> Number -> State
init startTime timePerFrame =
  { timeAtLastFrame: startTime
  , leftoverTime: timePerFrame
  , timePerFrame: timePerFrame
  }

update :: Number -> State -> Tuple Int State
update currentTime state =
  let
    timeSinceLastLogic = (currentTime - state.timeAtLastFrame) + state.leftoverTime

    framesToExecute = Int.floor $ timeSinceLastLogic / state.timePerFrame
  in
    Tuple framesToExecute
      $ state
          { leftoverTime = timeSinceLastLogic - ((Int.toNumber framesToExecute) * state.timePerFrame)
          , timeAtLastFrame = currentTime
          }
