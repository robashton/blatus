module Sisy.BuiltIn.Extensions.Explosions where

import Prelude
import Data.Filterable (filterMap)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Sisy.Runtime.Entity (EntityId)
import Sisy.Math (Point)

type ActiveExplosion
  = { location :: Point
    , velocity :: Point
    , age :: Int
    , owner :: EntityId
    }

type State
  = { explosions :: List ActiveExplosion }

init :: State
init = { explosions: Nil }

createExplosion :: EntityId -> Point -> Point -> State -> State
createExplosion owner location velocity state =
  state
    { explosions =
      { location
      , velocity
      , age: 0
      , owner
      }
        : state.explosions
    }

tick :: State -> State
tick state = state { explosions = filterMap updateExplosion state.explosions }

updateExplosion :: ActiveExplosion -> Maybe ActiveExplosion
updateExplosion e
  | e.age > 90 = Nothing
  | otherwise = Just $ e { age = e.age + 1 }
