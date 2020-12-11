module Pure.BuiltIn.Bullets where

import Prelude

import Data.Array (find, foldl)
import Data.Bifunctor (lmap, rmap)
import Data.List (List(..), snoc, (:))
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..), fst)
import Pure.Entity (Entity, EntityId)
import Pure.Math (Point)
import Pure.Runtime.Scene (Game)

type ActiveBullet = { location :: Point
                    , velocity :: Point
                    , age :: Int
                    , owner :: EntityId
                    , power :: Number
                    }


type BulletHit = { entity :: EntityId
                 , bullet :: ActiveBullet
                 }

type State ev = { bullets :: List ActiveBullet
                , liftEvent :: BulletHit -> ev
                }

init :: forall ev. (BulletHit -> ev) -> State ev
init liftEvent = { bullets : Nil, liftEvent }

fireBullet :: forall ev. EntityId -> Point -> Point -> Number -> State ev -> State ev
fireBullet owner location velocity power state = 
  state { bullets = { location
                         , velocity
                         , age: 0
                         , owner
                         , power } : state.bullets }

tick :: forall msg ev. (State ev) -> Game msg ev -> Tuple (State ev) (List ev)
tick state game = 
  lmap (\b -> state { bullets = b }) $ 
    foldl (\(Tuple acc evs) b -> 
      let
          res = (updateBullet state b game) 
       in
          lmap (maybe acc (snoc acc)) $ rmap (maybe evs (snoc evs)) res
       ) (Tuple Nil Nil) state.bullets

updateBullet :: forall msg ev. State ev -> ActiveBullet -> Game msg ev -> Tuple (Maybe ActiveBullet) (Maybe ev)
updateBullet state b g =
  if b.age > 150 then Tuple Nothing Nothing
  else 
    case (find (testBulletWithEntity b) g.entities) of
      Nothing -> Tuple (Just $ b { age = b.age + 1
                        , location = b.location + b.velocity
                        }) Nothing
      Just entity -> Tuple Nothing $ Just $ state.liftEvent { entity: entity.id, bullet: b } 
    
-- We'll just go with a sloppy circle test
-- for now, but in reality we will *need* a sweep test for line / aabb, just too lazy to do that now
testBulletWithEntity :: forall msg ev. ActiveBullet -> Entity msg ev -> Boolean
testBulletWithEntity bullet entity | bullet.owner == entity.id = false
                                   | otherwise = let distSq = (bullet.location.x - entity.location.x) * (bullet.location.x - entity.location.x)
                                                             + (bullet.location.y - entity.location.y) * (bullet.location.y - entity.location.y)
                                                     radius = (max entity.width entity.height) * 0.5
                                                   in
                                                      distSq < radius * radius
