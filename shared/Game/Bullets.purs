module Pure.Game.Bullets where

import Prelude

import Data.Exists (mkExists)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Pure.Behaviours.BasicBitchPhysics as BasicBitchPhysics
import Pure.Entity (Entity, EntityBehaviour(..), EntityClass(..), EntityId(..), HtmlColor(..), EntityCommandHandler, emptyEntity)
import Pure.Math (Point)
import Pure.Types (EntityCommand, GameEvent)

type ActiveBullet = { location :: Point
                    , velocity :: Point
                    }

type State = { bullets :: Array ActiveBullet
             , lastAllocatedIndex :: Int
             }

fireBullet :: EntityId -> Point -> Point -> State -> State
fireBullet id position velocity state = state

--tick :: forall msg ev. State -> Game msg ev -> Tuple Game (List ev)
--tick state game = Tuple game Nil

init :: State
init = { bullets : []
       , lastAllocatedIndex : 0 
       }
