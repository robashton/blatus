module Sisy.BuiltIn.Extensions.Bullets where

import Prelude
import Data.Bifunctor (lmap, rmap)
import Data.Foldable (find, foldl)
import Data.List (List(..), snoc, (:))
import Data.Maybe (Maybe(..), maybe)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Data.Variant (Variant, inj)
import Sisy.Math (Point, Rect)
import Sisy.Runtime.Entity (Entity, EntityId)
import Sisy.Runtime.Scene (Game)

type ActiveBullet
  = { location :: Point
    , velocity :: Point
    , age :: Int
    , owner :: EntityId
    , power :: Number
    }

type BulletHit
  = { entity :: EntityId
    , bullet :: ActiveBullet
    }

type Event
  = ( bulletHit :: BulletHit )

bulletHit :: BulletHit -> Variant Event
bulletHit = inj (SProxy :: SProxy "bulletHit")

type State
  = { bullets :: List ActiveBullet
    }

init :: State
init = { bullets: Nil }

fireBullet :: EntityId -> Point -> Point -> Number -> State -> State
fireBullet owner location velocity power state =
  state
    { bullets =
      { location
      , velocity
      , age: 0
      , owner
      , power
      }
        : state.bullets
    }

tick ::
  forall cmd ev entity.
  State -> Game cmd ev ( aabb :: Rect | entity ) -> Tuple State (List (Variant Event))
tick state game =
  lmap (\b -> state { bullets = b })
    $ foldl
        ( \(Tuple acc evs) b ->
            let
              res = (updateBullet state b game)
            in
              lmap (maybe acc (snoc acc)) $ rmap (maybe evs (snoc evs)) res
        )
        (Tuple Nil Nil)
        state.bullets

updateBullet ::
  forall cmd ev entity.
  State -> ActiveBullet -> Game cmd ev ( aabb :: Rect | entity ) -> Tuple (Maybe ActiveBullet) (Maybe (Variant Event))
updateBullet state b g =
  if b.age > 150 then
    Tuple Nothing Nothing
  else case (find (testBulletWithEntity b) g.entities) of
    Nothing ->
      Tuple
        ( Just
            $ b
                { age = b.age + 1
                , location = b.location + b.velocity
                }
        )
        Nothing
    Just entity -> Tuple Nothing $ Just $ bulletHit { entity: entity.id, bullet: b }

-- We'll just go with a sloppy circle test
-- for now, but in reality we will *need* a sweep test for line / aabb, just too lazy to do that now
testBulletWithEntity :: forall cmd ev entity. ActiveBullet -> Entity cmd ev ( aabb :: Rect | entity ) -> Boolean
testBulletWithEntity bullet entity
  | bullet.owner == entity.id = false
  | otherwise =
    let
      distSq =
        (bullet.location.x - entity.location.x) * (bullet.location.x - entity.location.x)
          + (bullet.location.y - entity.location.y)
          * (bullet.location.y - entity.location.y)

      radius = (max entity.aabb.width entity.aabb.height) * 0.5
    in
      distSq < radius * radius
