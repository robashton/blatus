module Pure.Game.Bullets where

import Prelude

import Data.Exists (mkExists)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Pure.Behaviours.BasicBitchPhysics as BasicBitchPhysics
import Pure.Entity (Entity, EntityBehaviour(..), EntityClass(..), EntityId(..), HtmlColor(..), EntityCommandHandler, emptyEntity)
import Pure.Math (Point)
import Pure.Types (EntityCommand, GameEvent)
import Data.Tuple (Tuple(..))
import Pure.Runtime.Scene (Game)
import Data.Sequence (Seq, cons)
import Data.Sequence as Seq

type ActiveBullet = { location :: Point
                    , velocity :: Point
                    , age :: Int
                    , owner :: EntityId
                    }

type State = { bullets :: Seq ActiveBullet
             }

fireBullet :: EntityId -> Point -> Point -> State -> State
fireBullet owner location velocity state = 
  state { bullets = cons { location
                         , velocity
                         , age: 0
                         , owner } state.bullets }

tick :: forall msg ev. State -> Game msg ev -> Tuple State (List ev)
tick state game = 
  Tuple (state { bullets = map (\b -> b { location = b.location + b.velocity
                                        , age = b.age + 1 
                                        }) state.bullets }) Nil

init :: State
init = { bullets : Seq.empty }

