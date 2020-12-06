module Pure.Behaviour where

import Prelude
import Pure.Entity as Entity
import Data.List ((:))
import Control.Monad.State (State)
import Control.Monad.State as State
import Pure.Math as Math

entity :: State Entity.BehaviourExecutionContext Entity.Entity
entity = State.gets _.entity

updateEntity  :: (Entity.Entity -> Entity.Entity) -> State Entity.BehaviourExecutionContext Unit
updateEntity f = 
  State.modify_ (\s -> s { entity = f s.entity })

raiseEvent  :: Entity.GameEvent -> State Entity.BehaviourExecutionContext Unit
raiseEvent ev =
  State.modify_ (\s@{ events } -> s { events = ev : events })


applyThrust :: Number -> Number -> State Entity.BehaviourExecutionContext Unit
applyThrust accel maxSpeed  = do
  e <- State.gets _.entity
  let updated = Entity.applyForce { direction: Math.rotationToVector e.rotation, force: accel } e
  State.modify_ (\s -> s { entity = updated })


rotate :: Number -> State Entity.BehaviourExecutionContext Unit
rotate amount = do
  State.modify_ (\s -> s { entity { rotation = s.entity.rotation + amount  } })

