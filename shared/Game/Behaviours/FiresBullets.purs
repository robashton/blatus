module Pure.Behaviours.FiresBullets where

import Prelude
import Data.Exists (Exists, mkExists)
import Data.Newtype (unwrap, wrap)
import Pure.Behaviour as B
import Pure.Entity (EntityBehaviour(..))
import Pure.Math (rotationToVector, scalePoint)
import Pure.Types (EntityCommand(..), GameEvent(..))

init :: forall entity. 
  { max :: Int, coolOffPeriod :: Int, speed :: Number, rate :: Int, power :: Number } 
  -> Exists (EntityBehaviour EntityCommand GameEvent entity)
init { max, speed, coolOffPeriod, rate, power } =
  mkExists
    $ EntityBehaviour
        { state:
            { firingTimer: 0
            , coolOffTimer: 0
            , bulletsFired: 0
            , power
            , firing: false
            }
        , handleCommand: handleCommand
        }
  where
  handleCommand command state@{ firingTimer } = do
    entity <- B.entity
    case command of
      StartFireBullet ->
        if state.coolOffTimer <= 0 then
          pure $ state { firing = true }
        else
          pure state
      StopFireBullet ->
        if state.firing then
          pure $ state { firing = false, coolOffTimer = coolOffPeriod, firingTimer = 0, bulletsFired = 0 }
        else
          pure state
      Tick ->
        if firingTimer <= 0 && state.firing && state.bulletsFired < max then do
          B.raiseEvent $ (BulletFired { owner: entity.id, location: location entity, velocity: velocity entity, power })
          pure $ state { firingTimer = rate, bulletsFired = state.bulletsFired + 1 }
        else if state.firing then
          pure $ state { firingTimer = state.firingTimer - 1 }
        else if (not state.firing) && state.coolOffTimer > 0 then
          pure $ state { coolOffTimer = state.coolOffTimer - 1 }
        else
          pure state
      _ -> pure state
    where
    direction entity = rotationToVector entity.rotation

    location entity = entity.location + (scalePoint entity.width $ direction entity)

    velocity entity = (scalePoint speed $ direction entity) -- + entity.velocity
