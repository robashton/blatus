module Pure.Behaviours.HasHealth where

import Prelude

import Data.Exists (Exists, mkExists)

import Pure.Entity (EntityBehaviour(..))
import Pure.Types (EntityCommand(..), GameEvent)

init :: Number -> Exists (EntityBehaviour EntityCommand GameEvent)
init amount = mkExists $ EntityBehaviour { state: amount
                                         , handleCommand:  \command state ->
                                                                  case command of 
                                                                       Damage damage -> pure $ state - damage
                                                                       _ -> pure state
                                                                   
                                               }
