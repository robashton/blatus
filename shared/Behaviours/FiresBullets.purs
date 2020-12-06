module Pure.Behaviours.FiresBullets where

import Prelude

import Data.Exists (Exists, mkExists)
import Pure.Behaviour as B
import Pure.Entity (EntityBehaviour(..), EntityCommand(..), GameEvent(..))
import Pure.Math (Rect, Point, scalePoint, rotationToVector, lerp)
import Data.Newtype (class Newtype, unwrap, wrap)

init :: { max :: Int, speed :: Number, rate:: Int } -> Exists EntityBehaviour
init { max, speed, rate } = mkExists $ EntityBehaviour { state: { current: 0, firingTimer: 0 }
                                                                , handleCommand: handleCommand
                                                         }
                                                       
             where handleCommand command state@{ current, firingTimer } = do
                                             entity <- B.entity 
                                             case command of
                                               FireBullet -> 
                                                 if firingTimer <= 0 then do
                                                   B.raiseEvent $ (BulletFired { id: id entity, location: location entity, velocity: velocity entity }) 
                                                   pure state { current = current + 1, firingTimer = rate }
                                                 else
                                                   pure state
                                               Tick -> pure $ state { current = if current > max then 0 else current, 
                                                                firingTimer = if firingTimer > 0 then firingTimer - 1 else firingTimer
                                                               }

                                               _ -> pure state
                                          where id entity = wrap $ (unwrap entity.id) <> "-bullet-" <> (show state.current)
                                                direction entity = rotationToVector entity.rotation
                                                location entity = entity.location + (scalePoint entity.width $ direction entity)
                                                velocity entity = (scalePoint speed $ direction entity) + entity.velocity
