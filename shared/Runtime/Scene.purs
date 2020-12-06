module Pure.Runtime.Scene where

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
import Pure.Entity (Entity, EntityId)
import Pure.Entity as Entity
import Pure.Math (Point, Rect)

type EntityMap cmd ev = Map EntityId (Entity cmd ev)

type Game cmd ev =  { entities  :: (EntityMap cmd ev)
                    , world :: Rect
                    , tickMsg :: cmd
                    , onTicked :: List (TickState cmd ev -> TickState cmd ev)
                    }

initialModel :: forall cmd ev. cmd -> Game cmd ev
initialModel tickMsg = { entities : mempty
                       , world:  { x: -1000.0, y: -1000.0, width: 2000.0, height: 2000.0 }
                       , tickMsg
                       , onTicked : Nil
                       }

onTick :: forall cmd ev. (TickState cmd ev -> TickState cmd ev) -> Game cmd ev -> Game cmd ev
onTick h game = 
  game { onTicked = h : game.onTicked }

sendCommand :: forall cmd ev. EntityId -> cmd -> Game cmd ev -> Tuple (Game cmd ev) (List ev)
sendCommand id command game@{ entities } =
  case (Map.lookup id entities) of
    Nothing -> Tuple game Nil
    Just entity -> let (Tuple newEntity evs) = Entity.processCommand entity command 
                   in Tuple (game { entities = Map.insert id newEntity entities }) evs

discardEvents :: forall cmd ev. Tuple (Game cmd ev) (List ev) -> Game cmd ev
discardEvents (Tuple game _events) = game


addEntity :: forall cmd ev. Entity cmd ev -> Game cmd ev -> Game cmd ev
addEntity entity game =
  game { entities = Map.insert entity.id entity game.entities }

updateEntity :: forall cmd ev. (Entity cmd ev -> Entity cmd ev) -> EntityId -> Game cmd ev -> Game cmd ev
updateEntity fn id game =
  game { entities = Map.update (fn >>> Just) id game.entities }

removeEntity :: forall cmd ev. EntityId -> Game cmd ev -> Game cmd ev
removeEntity id game =
  game { entities = Map.delete id game.entities }


entityById :: forall cmd ev. EntityId -> Game cmd ev -> Maybe (Entity cmd ev)
entityById id { entities } =
  find (\e -> e.id == id) entities

                                             
type TickState cmd ev =  { entities :: Map.Map Int (Entity cmd ev)
                         , events :: List (List ev)
                         , entityCount :: Int
                         , entityRange :: Array Int
                         }

tick :: forall cmd ev. Game cmd ev -> Tuple (Game cmd ev) (List ev)
tick game =
  Tuple (game { entities = foldl (\m e -> Map.insert e.id e m) mempty (Map.values finalState.entities) }) $ concat finalState.events
  where
        finalState = foldl (\is fn -> fn is) tickedState game.onTicked
        tickedState = foldl (tickEntity game.tickMsg) initialState entityRange 
        initialState = { entities
                       , events: Nil
                       , entityRange
                       , entityCount
                       }
        entityRange = Array.range 0 entityCount
        entityCount = Map.size entities
        entities = foldlWithIndex (\i m (Tuple k v) -> Map.insert i v m) mempty $ (Map.toUnfoldableUnordered game.entities :: List (Tuple EntityId (Entity cmd ev)))

tickEntity :: forall cmd ev. cmd -> TickState cmd ev -> Int -> TickState cmd ev
tickEntity tickMsg state index = 
  maybe state (\e -> let
                        (Tuple newEntity newEvents) = Entity.processCommand e tickMsg 
                      in
                        state { events = newEvents : state.events
                              , entities = Map.insert index newEntity state.entities
                              }
              ) $ Map.lookup index state.entities
