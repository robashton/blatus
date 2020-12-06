module Pure.Behaviours.HasHealth where

import Prelude

import Data.Exists (Exists, mkExists)
import Pure.Behaviour as B
import Pure.Entity (EntityBehaviour(..), EntityCommand(..))
import Pure.Math (scalePoint)

init :: Number -> Exists EntityBehaviour 
init amount = mkExists $ EntityBehaviour { state: amount
                                         , handleCommand:  \command state ->
                                                                  case command of 
                                                                       Damage damage -> pure $ state - damage
                                                                       _ -> pure state
                                                                   
                                               }
