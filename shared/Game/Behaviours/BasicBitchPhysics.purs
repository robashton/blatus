module Pure.Behaviours.BasicBitchPhysics where

import Prelude
import Control.Monad.State (State)
import Control.Monad.State as State
import Data.Exists (Exists, mkExists)
import Prim.Row as Row
import Pure.Behaviour as B
import Pure.Entity (Entity, EntityRow, EntityBehaviour(..), BehaviourExecutionContext)
import Pure.Math (Point, scalePoint)
import Pure.Math as Math
import Pure.Types (EntityCommand(..), GameEvent)

init ::
  forall entity.
  Exists (EntityBehaviour EntityCommand GameEvent (Required entity))
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

applyForce ::
  forall cmd ev entity.
  { direction :: Point, force :: Number } -> Entity cmd ev (Required entity) -> Entity cmd ev (Required entity)
applyForce { direction, force } entity@{ velocity, mass } = entity { velocity = velocity + (scalePoint (force / mass) direction) }

applyThrust ::
  forall cmd ev entity.
  Number -> Number -> State (BehaviourExecutionContext cmd ev (Required entity)) Unit
applyThrust accel maxSpeed = do
  e <- State.gets _.entity
  let
    updated = applyForce { direction: Math.rotationToVector e.rotation, force: accel } e
  State.modify_ (\s -> s { entity = updated })

type Required r
  = ( mass :: Number
    , velocity :: Point
    , friction :: Number
    | r
    )

