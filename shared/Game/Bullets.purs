module Pure.Game.Bullets where

import Prelude

import Data.Array (find, foldl)
import Data.Bifunctor (lmap)
import Data.Exists (mkExists)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), fst)
import Pure.Behaviours.BasicBitchPhysics as BasicBitchPhysics
import Pure.Entity (Entity, EntityBehaviour(..), EntityClass(..), EntityId(..), HtmlColor(..), EntityCommandHandler, emptyEntity)
import Pure.Math (Point)
import Pure.Runtime.Scene (Game)
import Pure.Types (EntityCommand, GameEvent)

type ActiveBullet = { location :: Point
                    , velocity :: Point
                    , age :: Int
                    , owner :: EntityId
                    }

type State = { bullets :: List ActiveBullet
             }

fireBullet :: EntityId -> Point -> Point -> State -> State
fireBullet owner location velocity state = 
  state { bullets = { location
                         , velocity
                         , age: 0
                         , owner } : state.bullets }

tick :: forall msg ev. State -> Game msg ev -> Tuple State (List ev)
tick state game = 
  lmap (\b -> state { bullets = b }) $ 
    foldl (\(Tuple acc evs) b -> 
      let
          res = (updateBullet b game) 
          nbs = case (fst res) of 
                  Nothing -> acc
                  Just nb -> nb : acc
       in Tuple nbs evs) (Tuple Nil Nil) state.bullets

updateBullet :: forall msg ev. ActiveBullet -> Game msg ev -> Tuple (Maybe ActiveBullet) (Maybe ev)
updateBullet b g =
  if b.age > 150 then Tuple Nothing Nothing
  else 
    case (find (testBulletWithEntity b) g.entities) of
      Nothing -> Tuple (Just $ b { age = b.age + 1
                        , location = b.location + b.velocity
                        }) Nothing
      Just entity -> Tuple Nothing Nothing -- raise event here
    
-- We'll just go with sloppy circle tests
-- for now, but in reality we will *need* a sweep test for line / aabb, just too lazy to do that now
testBulletWithEntity :: forall msg ev. ActiveBullet -> Entity msg ev -> Boolean
testBulletWithEntity bullet entity = 
  let distSq = (bullet.location.x - entity.location.x) * (bullet.location.x - entity.location.x)
             + (bullet.location.y - entity.location.y) * (bullet.location.y - entity.location.y)
      radius = (max entity.width entity.height) * 0.5
   in
      distSq < radius * radius


init :: State
init = { bullets : Nil }

