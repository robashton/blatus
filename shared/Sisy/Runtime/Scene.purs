module Sisy.Runtime.Scene where

import Prelude
import Data.Array as Array
import Data.Foldable (foldl)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.List (List(..), concat, (:))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Symbol (SProxy(..))
import Data.Traversable (find)
import Data.Tuple (Tuple(..))
import Data.Variant (Variant, inj)
import Sisy.Runtime.Entity (Entity, EntityId, Cmd)
import Sisy.Runtime.Entity as Entity
import Sisy.Math (Rect)

type EntityMap cmd ev entity
  = Map EntityId (Entity cmd ev entity)

type TickState (cmd :: Row Type) ev entity
  = { entities :: Map.Map Int (Entity cmd ev entity)
    , events :: List (List (Variant ev))
    , entityCount :: Int
    , entityRange :: Array Int
    }

type Game cmd ev entity
  = { entities :: (EntityMap cmd ev entity)
    , world :: Rect
    , onTicked :: List (TickState cmd ev entity -> TickState cmd ev entity)
    }

tickCmd :: forall cmd. Variant (Cmd cmd)
tickCmd = inj (SProxy :: SProxy "tick") unit

initialModel :: forall cmd ev entity. Game cmd ev entity
initialModel =
  { entities: Map.empty
  , world: { x: -1250.0, y: -1250.0, width: 2500.0, height: 2500.0 }
  , onTicked: Nil
  }

onTick :: forall cmd ev entity. (TickState cmd ev entity -> TickState cmd ev entity) -> Game cmd ev entity -> Game cmd ev entity
onTick h game = game { onTicked = h : game.onTicked }

sendCommand :: forall cmd ev entity. EntityId -> Variant (Cmd cmd) -> Game cmd ev entity -> Tuple (Game cmd ev entity) (List (Variant ev))
sendCommand id command game@{ entities } = case (Map.lookup id entities) of
  Nothing -> Tuple game Nil
  Just entity ->
    let
      (Tuple newEntity evs) = Entity.processCommand entity command
    in
      Tuple (game { entities = Map.insert id newEntity entities }) evs

discardEvents :: forall cmd ev entity. Tuple (Game cmd ev entity) (List (Variant ev)) -> Game cmd ev entity
discardEvents (Tuple game _events) = game

addEntity :: forall cmd ev entity. Entity cmd ev entity -> Game cmd ev entity -> Game cmd ev entity
addEntity entity game = game { entities = Map.insert entity.id entity game.entities }

updateEntity :: forall cmd ev entity. (Entity cmd ev entity -> Entity cmd ev entity) -> EntityId -> Game cmd ev entity -> Game cmd ev entity
updateEntity fn id game = game { entities = Map.update (fn >>> Just) id game.entities }

removeEntity :: forall cmd ev entity. EntityId -> Game cmd ev entity -> Game cmd ev entity
removeEntity id game = game { entities = Map.delete id game.entities }

entityById :: forall cmd ev entity. EntityId -> Game cmd ev entity -> Maybe (Entity cmd ev entity)
entityById id { entities } = find (\e -> e.id == id) entities

tick ::
  forall cmd ev entity.
  Game cmd ev entity -> Tuple (Game cmd ev entity) (List (Variant ev))
tick game = Tuple (game { entities = foldl (\m e -> Map.insert e.id e m) Map.empty (Map.values finalState.entities) }) $ concat finalState.events
  where
  finalState = foldl (\is fn -> fn is) tickedState game.onTicked

  tickedState = foldl (tickEntity tickCmd) initialState entityRange

  initialState =
    { entities
    , events: Nil
    , entityRange
    , entityCount
    }

  entityRange = Array.range 0 entityCount

  entityCount = Map.size entities

  entities = foldlWithIndex (\i m (Tuple k v) -> Map.insert i v m) Map.empty $ (Map.toUnfoldableUnordered game.entities :: List (Tuple EntityId (Entity cmd ev entity)))

tickEntity ::
  forall cmd ev entity.
  Variant (Cmd cmd) -> TickState cmd ev entity -> Int -> TickState cmd ev entity
tickEntity cmd state index =
  maybe state
    ( \e ->
        let
          (Tuple newEntity newEvents) = Entity.processCommand e cmd
        in
          state
            { events = newEvents : state.events
            , entities = Map.insert index newEntity state.entities
            }
    )
    $ Map.lookup index state.entities
