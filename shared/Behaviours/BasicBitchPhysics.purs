module Pure.Behaviours.BasicBitchPhysics where

import Prelude

import Data.Exists (Exists, mkExists)
import Pure.Behaviour as B
import Pure.Entity (EntityBehaviour(..), EntityCommand(..))
import Pure.Math (scalePoint)


init :: Exists EntityBehaviour
init = mkExists $ EntityBehaviour { state: unit
                                  , handleCommand:  \command _ -> case command of 
                                                                    Tick -> do
                                                                      B.updateEntity (\e@{ location, velocity, friction }  -> 
                                                                        e { location = location + velocity, velocity = scalePoint friction velocity })
                                                                    _ -> pure unit }

