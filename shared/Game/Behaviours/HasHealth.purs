module Pure.Behaviours.HasHealth where

import Prelude

import Data.Exists (Exists, mkExists)

import Pure.Behaviour as B
import Pure.Entity (EntityBehaviour(..))
import Pure.Types (EntityCommand(..), GameEvent(..))

init :: Number -> Exists (EntityBehaviour EntityCommand GameEvent)
init amount = mkExists $ EntityBehaviour { state: amount
                                         , handleCommand:  \command state ->
                                                                  case command of 
                                                                       Damage damage -> do
                                                                          let health = state - damage.amount
                                                                          entity <- B.entity 
                                                                          if health <= 0.0 then do
                                                                            B.raiseEvent $ EntityDestroyed { entity: entity.id, destroyer: damage.source }
                                                                            pure health
                                                                          else
                                                                            pure health
                                                                       _ -> pure state
                                                                   
                                               }
