module Pure.Behaviour where

import Prelude
import Control.Monad.State (State)
import Control.Monad.State as State
import Data.List ((:))
import Data.Variant (Variant)
import Pure.Entity (EntityId)
import Pure.Entity as Entity

id :: forall cmd ev entity. State (Entity.BehaviourExecutionContext cmd ev entity) EntityId
id = _.id <$> State.gets _.entity

entity :: forall cmd ev entity. State (Entity.BehaviourExecutionContext cmd ev entity) (Entity.Entity cmd ev entity)
entity = State.gets _.entity

updateEntity :: forall cmd ev entity. (Entity.Entity cmd ev entity -> Entity.Entity cmd ev entity) -> State (Entity.BehaviourExecutionContext cmd ev entity) Unit
updateEntity f = State.modify_ (\s -> s { entity = f s.entity })

raiseEvent :: forall cmd ev entity. Variant ev -> State (Entity.BehaviourExecutionContext cmd ev entity) Unit
raiseEvent ev = State.modify_ (\s@{ events } -> s { events = ev : events })

rotate :: forall cmd ev entity. Number -> State (Entity.BehaviourExecutionContext cmd ev entity) Unit
rotate amount = do
  State.modify_ (\s -> s { entity { rotation = s.entity.rotation + amount } })
