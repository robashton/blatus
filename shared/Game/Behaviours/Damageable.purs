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
                                                   let health = entity.health - damage.amount
                                                   B.updateEntity (\e -> e { health = health })
                                                   if health <= 0.0 then do
                                                     B.raiseEvent $ EntityDestroyed { entity: entity.id, destroyer: damage.source }
                                                     pure unit
                                                   else
                                                       pure unit
                                                _ -> pure unit

                                               }
