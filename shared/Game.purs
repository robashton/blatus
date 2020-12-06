module Pure.Game where

-- Rough plan :: this is  the shared model, and an instance of it is executed per 'game'
-- On the server that means a gen-server, on the client it means once per player in that game
-- Commands sent to the player entity, are sent to the server and re-written to be sent to the appropriate entity
-- both on server model and other player's client models
-- Events for significant decisions should be raised by all models, events sent by client model can be ignored
-- Events sent from server model to clients (and indeed to itself) are used to enforce the critical decisions
-- That means things like collisions causing health changes (ButtetHitPlayer), and health getting less than 0 (PlayerDestroyed)

import Prelude

import Control.Apply (lift2)
import Data.Array as Array
import Data.Foldable (foldl, class Foldable)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.List (List(..), concat, foldr, (:))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Traversable (find)
import Data.Tuple (Tuple(..))
import Math (pow, sqrt) as Math
import Pure.Entity (Entity, EntityCommand(..), EntityId, GameEvent(..))
import Pure.Entity as Entity
import Pure.Math (Point, Rect)

import Pure.Entities.Bullet as Bullet

type EntityMap = Map EntityId Entity

type Game =  { entities  :: EntityMap
             , world :: Rect
  }

initialModel :: Game
initialModel = { entities : mempty
               , world:  { x: -1000.0, y: -1000.0, width: 2000.0, height: 2000.0 }
}

sendCommand :: EntityId -> EntityCommand -> Game -> Tuple Game (List GameEvent)
sendCommand id command game@{ entities } =
  case (Map.lookup id entities) of
    Nothing -> Tuple game Nil
    Just entity -> let (Tuple newEntity evs) = Entity.processCommand entity command 
                   in Tuple (game { entities = Map.insert id newEntity entities }) evs

foldEvents :: forall m. Foldable m => Game -> (m GameEvent) -> Game
foldEvents game events = foldr sendEvent game events

discardEvents :: Tuple Game (List GameEvent) -> Game
discardEvents (Tuple game _events) = game

sendEvent :: GameEvent -> Game -> Game
sendEvent ev game = 
  case ev of
       BulletFired deets -> 
         addEntity (Bullet.init deets.id deets.location deets.velocity) game
       EntityCollided _ -> game

addEntity :: Entity -> Game -> Game
addEntity entity game =
  game { entities = Map.insert entity.id entity game.entities }

updateEntity :: (Entity -> Entity) -> EntityId -> Game -> Game
updateEntity fn id game =
  game { entities = Map.update (fn >>> Just) id game.entities }

removeEntity :: EntityId -> Game -> Game
removeEntity id game =
  game { entities = Map.delete id game.entities }


entityById :: EntityId -> Game -> Maybe Entity
entityById id { entities } =
  find (\e -> e.id == id) entities

                                             
type TickState = { entities :: Map.Map Int Entity
                 , events :: List (List GameEvent)
                 }
-- entities = Map.fromFoldable final.entities  

tick :: Game -> Tuple Game (List GameEvent)
tick game@ { entities } =
  Tuple (game { entities = foldl (\m e -> Map.insert e.id e m) mempty (Map.values final.entities) }) $ concat final.events
  where
        final = foldl (collideEntity length)  tickedState range
        tickedState = foldl tickEntity initialState range
        range = Array.range 0 length
        length = Map.size initialState.entities
        initialState = { entities: foldlWithIndex (\i m (Tuple k v) -> Map.insert i v m) mempty $ (Map.toUnfoldableUnordered entities :: List (Tuple EntityId Entity))
                       , events: Nil
                       }

tickEntity :: TickState -> Int -> TickState 
tickEntity state index = 
  maybe state (\e -> let
                        (Tuple newEntity newEvents) = Entity.processCommand e Tick 
                      in
                        state { events = newEvents : state.events
                              , entities = Map.insert index newEntity state.entities
                              }
              ) $ Map.lookup index state.entities
--  

collideEntity :: Int -> TickState -> Int -> TickState
collideEntity termination state index 
  | index == termination = state
  | otherwise = foldl (collidePair index) state $ Array.range (index+1) termination

collidePair :: Int -> TickState -> Int -> TickState
collidePair li state ri =
  fromMaybe state $ lift2 (\left right ->
    if squareCheck left right then
      let lf = (magnitude left.velocity) * left.mass
          rf = (magnitude right.velocity) * right.mass
          ur = Entity.applyForce { direction: (vectorBetween left.location right.location), force: lf } right -- force from left to right, applied to right
          ul = Entity.applyForce { direction: (vectorBetween right.location left.location), force: rf } left -- force from right to left, applied to left
       in
         state { entities = Map.insert ri ur $ Map.insert li ul state.entities
               , events = (EntityCollided { one: left.id, two: right.id, force: lf }
                         : EntityCollided { one: right.id, two: left.id, force: rf }
                         : Nil) : state.events 
               }
    else
      state) (Map.lookup li state.entities) (Map.lookup ri state.entities)
        

vectorBetween :: Point -> Point -> Point
vectorBetween s d =
  normalise (d - s)

normalise :: Point -> Point
normalise point@{ x, y } = { x: x / den, y: y / den }
  where den = magnitude point

magnitude :: Point -> Number
magnitude {x, y} = Math.sqrt $ (x * x) + (y * y)

squareCheck :: Entity -> Entity -> Boolean
squareCheck inner subject 
  | inner.location.x > subject.location.x + subject.width = false
  | inner.location.y > subject.location.y + subject.height = false
  | subject.location.x > inner.location.x + inner.width = false
  | subject.location.y > inner.location.y + inner.height = false
  | otherwise = true

circleCheck :: Entity -> Entity -> Boolean
circleCheck inner subject = 
  let distSq = (Math.pow (inner.location.x - subject.location.x) 2.0)
             + (Math.pow (inner.location.y - subject.location.y) 2.0) 
      combinedSizeSq = Math.pow ((max inner.width inner.height) / 2.0 + (max subject.width subject.height) / 2.0) 2.0
   in distSq < combinedSizeSq 


