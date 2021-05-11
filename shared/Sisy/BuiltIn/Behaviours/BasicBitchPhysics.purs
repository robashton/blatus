module Sisy.BuiltIn.Behaviours.BasicBitchPhysics where

import Prelude
import Control.Monad.State (State)
import Control.Monad.State as State
import Data.Exists (Exists, mkExists)
import Data.Ord (abs)
import Data.Variant (default, onMatch)
import Sisy.Math (Point, Rect, centreRect, scalePoint)
import Sisy.Math as Math
import Sisy.Runtime.Behaviour as B
import Sisy.Runtime.Entity (BehaviourExecutionContext, Entity, EntityBehaviour(..))
import Sisy.BuiltIn (Mass(..))

velocityThreshold :: Number
velocityThreshold = 0.0000001

init ::
  forall entity cmd ev.
  Exists (EntityBehaviour (Command cmd) ev (Required entity))
init =
  mkExists
    $ EntityBehaviour
        { state: unit
        , handleCommand:
            \command _ ->
              onMatch
                { tick:
                    \_ ->
                      B.updateEntity
                        ( \e@{ location, velocity, friction, aabb } ->
                            if (abs velocity.x) > velocityThreshold || (abs velocity.y) > velocityThreshold then
                              e
                                { location = location + velocity
                                , velocity = scalePoint friction velocity
                                , aabb = centreRect location aabb
                                }
                            else
                              e
                        )
                }
                (default (pure unit))
                command
        }

applyForce ::
  forall cmd ev entity.
  { direction :: Point, force :: Number } -> Entity cmd ev (Required entity) -> Entity cmd ev (Required entity)
applyForce { direction, force } entity@{ velocity, mass: Infinite } = entity

applyForce { direction, force } entity@{ velocity, mass: NoMass } = entity

applyForce { direction, force } entity@{ velocity, mass: Fixed mass } = entity { velocity = velocity + (scalePoint (force / mass) direction) }

applyThrust ::
  forall cmd ev entity.
  Number -> Number -> State (BehaviourExecutionContext cmd ev (Required entity)) Unit
applyThrust accel maxSpeed = do
  e <- State.gets _.entity
  let
    updated = applyForce { direction: Math.rotationToVector e.rotation, force: accel } e
  State.modify_ (\s -> s { entity = updated })

type Required r
  = ( mass :: Mass
    , velocity :: Point
    , friction :: Number
    , aabb :: Rect
    | r
    )

type Command :: forall k. k -> k
type Command r
  = ( | r )
