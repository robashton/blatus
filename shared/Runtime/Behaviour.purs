module Pure.Behaviour where

import Prelude
import Control.Monad.State (State)
import Control.Monad.State as State
import Data.List ((:))
import Pure.Entity (EntityId)
import Pure.Entity as Entity
import Pure.Math as Math

id :: forall cmd ev. State (Entity.BehaviourExecutionContext cmd ev) EntityId
id = _.id <$> State.gets _.entity

entity :: forall cmd ev. State (Entity.BehaviourExecutionContext cmd ev) (Entity.Entity cmd ev)
entity = State.gets _.entity

updateEntity :: forall cmd ev. (Entity.Entity cmd ev -> Entity.Entity cmd ev) -> State (Entity.BehaviourExecutionContext cmd ev) Unit
updateEntity f = State.modify_ (\s -> s { entity = f s.entity })

raiseEvent :: forall cmd ev. ev -> State (Entity.BehaviourExecutionContext cmd ev) Unit
raiseEvent ev = State.modify_ (\s@{ events } -> s { events = ev : events })

applyThrust :: forall cmd ev. Number -> Number -> State (Entity.BehaviourExecutionContext cmd ev) Unit
applyThrust accel maxSpeed = do
  e <- State.gets _.entity
  let
    updated = Entity.applyForce { direction: Math.rotationToVector e.rotation, force: accel } e
  State.modify_ (\s -> s { entity = updated })

rotate :: forall cmd ev. Number -> State (Entity.BehaviourExecutionContext cmd ev) Unit
rotate amount = do
  State.modify_ (\s -> s { entity { rotation = s.entity.rotation + amount } })
