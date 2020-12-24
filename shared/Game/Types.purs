module Pure.Types where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import GenericJSON (writeTaggedSumRep, taggedSumRep)
import Pure.Entity (EntityId)
import Pure.BuiltIn.Bullets as Bullets
import Pure.Math (Point)
import Simple.JSON (class ReadForeign, class WriteForeign)

data EntityCommand = Damage Number 
                   | Tick 
                   | PushForward 
                   | PushBackward 
                   | TurnLeft 
                   | TurnRight
                   | FireBullet
                   | StopPushForward
                   | StopPushBackward
                   | StopTurnLeft
                   | StopTurnRight
                   | StopFiring
                   | UpdateServerState { location :: Point
                                       , velocity :: Point
                                       , rotation :: Number
                                       }

derive instance genericEntityCommand :: Generic EntityCommand _
instance showEntityCommand :: Show EntityCommand where
  show = genericShow
instance writeForeignEntityCommand :: WriteForeign EntityCommand where
  writeImpl = writeTaggedSumRep
instance readForeignEntityCommand :: ReadForeign EntityCommand where
  readImpl = taggedSumRep
derive instance eqEntityCommand :: Eq EntityCommand


data GameEvent = BulletFired { id :: EntityId, location :: Point, velocity :: Point, power :: Number }
               | EntityCollided  { left :: EntityId, right :: EntityId, force :: Number } 
               | BulletHit Bullets.BulletHit
               | EntityDestroyed EntityId

derive instance genericGameEvent :: Generic GameEvent _
instance showGameEvent :: Show GameEvent where
  show = genericShow
instance writeForeignGameEvent :: WriteForeign GameEvent where
  writeImpl = writeTaggedSumRep
instance readForeignGameEvent :: ReadForeign GameEvent where
  readImpl = taggedSumRep
