module Pure.BuiltIn.Explosions where

import Prelude

import Data.Array (find, foldl)
import Data.Bifunctor (lmap)
import Data.Filterable (filterMap)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), fst)
import Pure.Entity (Entity, EntityId)
import Pure.Math (Point)
import Pure.Runtime.Scene (Game)

-- Might make this a particle system, might stylise it
-- as animations, shrug.

type ActiveExplosion = { location :: Point
                       , velocity :: Point
                       , age :: Int
                       , owner :: EntityId
                       }


type State = { explosions :: List ActiveExplosion }

init :: State 
init = { explosions : Nil }

createExplosion :: EntityId -> Point -> Point -> State -> State
createExplosion owner location velocity state = 
  state { explosions = { location
                       , velocity
                       , age: 0
                       , owner } : state.explosions }

tick :: State -> State
tick state = state { explosions = filterMap updateExplosion state.explosions }


updateExplosion :: ActiveExplosion -> Maybe ActiveExplosion
updateExplosion e 
  | e.age > 90 = Nothing
  | otherwise = Just $ e { age = e.age + 1 }

