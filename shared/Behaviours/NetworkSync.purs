module Pure.Behaviours.NetworkSync where

import Prelude

import Data.Exists (Exists, mkExists)
import Pure.Behaviour as B
import Pure.Entity (EntityBehaviour(..), EntityCommand(..))
import Pure.Math (lerp)

type ElasticConfig = { force :: Number }

init :: ElasticConfig -> Exists EntityBehaviour
init c = mkExists $ EntityBehaviour { state: { force: c.force
                                                    , location: { x: 0.0, y: 0.0 }
                                                    , rotation: 0.0
                                                    , velocity: { x: 0.0, y: 0.0 }
                                                    , oldLocation: { x: 0.0, y: 0.0 }
                                                    , oldRotation: 0.0
                                                    }
                                           , handleCommand: handleCommand
                                           }
                 where handleCommand command s = do
                         e <- B.entity 
                         case command of
                           UpdateServerState ss ->
                             pure $ s { oldLocation = e.location
                                      , oldRotation = e.rotation
                                      , location = ss.location
                                      , rotation = ss.rotation
                                      }
                           Tick -> do 
                             let targetRotation = s.rotation + (e.rotation - s.oldRotation)
                                 targetLocation = s.location + (e.location - s.oldLocation)
                                 newLocation = lerp e.location targetLocation s.force
                                 newRotation = e.rotation + s.force * (targetRotation - e.rotation) 
                             _ <- B.updateEntity (\entity -> entity { location = newLocation
                                                                    , rotation = newRotation
                                                                    })
                             pure s { rotation = targetRotation
                                    , location = targetLocation
                                    , oldRotation = newRotation
                                    , oldLocation = newLocation
                                    }
                           _ -> pure s

