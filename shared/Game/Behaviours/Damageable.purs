module Pure.Behaviours.Damageable where

import Prelude

import Data.Exists (Exists, mkExists)

import Pure.Behaviour as B
import Pure.Entity (EntityBehaviour(..))
import Pure.Types (EntityCommand(..), GameEvent(..))


init :: Exists (EntityBehaviour EntityCommand GameEvent)
init = mkExists $ EntityBehaviour { state: unit
                                  , handleCommand:  \command _ ->
                                           case command of 
                                                Damage damage -> do
                                                   entity <- B.entity 
                                                   let health = if entity.shield <= 0.0 then entity.health - damage.amount else entity.health
                                                       shield = if entity.shield > 0.0 then entity.shield - damage.amount else 0.0
                                                   B.updateEntity (\e -> e { health = health, shield = shield })
                                                   if health <= 0.0 then do
                                                     B.raiseEvent $ EntityDestroyed { entity: entity.id, destroyer: damage.source }
                                                     pure unit
                                                   else
                                                       pure unit
                                                _ -> pure unit

                                               }
