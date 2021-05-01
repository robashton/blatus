module Pure.Behaviour where

import Prelude
import Control.Monad.State (State)
import Control.Monad.State as State
import Data.List ((:))
import Pure.Entity (EntityId)
import Pure.Entity as Entity
import Pure.Math as Math

id :: forall cmd ev entity. State (Entity.BehaviourExecutionContext cmd ev entity) EntityId
id = _.id <$> State.gets _.entity

entity :: forall cmd ev entity. State (Entity.BehaviourExecutionContext cmd ev entity) (Entity.Entity cmd ev entity)
entity = State.gets _.entity

updateEntity :: forall cmd ev entity. (Entity.Entity cmd ev entity -> Entity.Entity cmd ev entity) -> State (Entity.BehaviourExecutionContext cmd ev entity) Unit
updateEntity f = State.modify_ (\s -> s { entity = f s.entity })

raiseEvent :: forall cmd ev entity. ev -> State (Entity.BehaviourExecutionContext cmd ev entity) Unit
raiseEvent ev = State.modify_ (\s@{ events } -> s { events = ev : events })

applyThrust :: forall cmd ev entity. Number -> Number -> State (Entity.BehaviourExecutionContext cmd ev entity) Unit
applyThrust accel maxSpeed = do
  e <- State.gets _.entity
  let
    updated = Entity.applyForce { direction: Math.rotationToVector e.rotation, force: accel } e
  State.modify_ (\s -> s { entity = updated })

rotate :: forall cmd ev entity. Number -> State (Entity.BehaviourExecutionContext cmd ev entity) Unit
rotate amount = do
  State.modify_ (\s -> s { entity { rotation = s.entity.rotation + amount } })
