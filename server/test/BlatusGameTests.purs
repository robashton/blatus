module Test.BlatusGameTests where

import Prelude
import Blatus.Entities.Behaviours.ProvidesResource (entityDestroyed)
import Blatus.Entities.Types (CollectableType(..), EntityClass(..))
import Blatus.Main as Main
import Blatus.Types (GameEvent)
import Control.Monad.Free (Free)
import Control.Monad.List.Trans (catMaybes)
import Data.Filterable (filterMap)
import Data.Foldable (any, foldl, null)
import Data.List (List(..), head, (:))
import Data.Maybe (Maybe(..), isJust)
import Data.Tuple (Tuple(..), fst, snd, uncurry)
import Data.Variant (Variant, default, onMatch)
import Erl.Test.EUnit (TestF, suite, test)
import Sisy.BuiltIn.Extensions.Collider (entityCollided)
import Sisy.Runtime.Entity (EntityId(..))
import Sisy.Runtime.Scene (entityById)
import Test.Assert (assertEqual)

bob :: EntityId
bob = EntityId "bob"

rock :: EntityId
rock = EntityId "rock"

runWhileEvents :: Tuple Main.State (List (Variant GameEvent)) -> Main.State
runWhileEvents (Tuple state evs)
  | null evs = state
  | otherwise = runWhileEvents $ foldl (\acc ev -> uncurry (\ng nevs -> Tuple ng $ (snd acc) <> nevs) $ Main.handleEvent (fst acc) ev) (Tuple state Nil) evs

tests :: Free TestF Unit
tests = do
  suite "Blatus Game Tests" do
    suite "Adding a player" do
      let
        originalState = Main.addPlayer bob $ Main.init 0.0

        Tuple stateWithTick evs = Main.doTick originalState
      test "Results in the player spawn being queued" do
        assertEqual
          { expected: { ticks: 0, playerId: bob } : Nil
          , actual: originalState.pendingSpawns
          }
      test "Results in the player spawning on the next tick" do
        let
          playerSpawned = any (default false # onMatch { playerSpawn: \s -> s.id == bob }) evs
        assertEqual
          { expected: true
          , actual: playerSpawned
          }
      test "Results in the spawn clearing from the queue" do
        assertEqual
          { expected: Nil
          , actual: stateWithTick.pendingSpawns
          }
      test "Results in the player being in the scene" do
        let
          final = runWhileEvents $ Main.doTick originalState
        assertEqual
          { expected: true
          , actual: isJust $ entityById bob final.scene
          }
    suite "Player with collectable in scene" do
      let
        originalState =
          runWhileEvents
            $ Main.doTick
            $ Main.addEntity
                { id: EntityId "rock"
                , class: Collectable { width: 10.0, height: 10.0, collectableType: Rock 100 }
                , location: { x: 0.0, y: 0.0 }
                , velocity: { x: 0.0, y: 0.0 }
                , rotation: 0.0
                , shield: 100.0
                , health: 100.0
                }
            $ Main.addPlayer bob
            $ Main.init 0.0
      suite "Player collides with collectable" do
        let
          Tuple newState evs = Main.handleEvent originalState (entityCollided { force: 0.0, left: rock, right: bob })
        test "Collectable destruction event raised" do
          let
            collectableDestroyed = any (default false # onMatch { entityDestroyed: \ev -> ev.entity == rock }) evs
          assertEqual
            { expected: true
            , actual: collectableDestroyed
            }
        test "Resource event gets raised for player" do
          let
            resourceProvided = head $ filterMap (default Nothing # onMatch { resourceProvided: \ev -> Just ev }) evs
          assertEqual
            { expected: Just { to: bob, resource: Rock 100 }
            , actual: resourceProvided
            }

--eventExists :: List (Variant GameEvent) -> Boolean
--eventExists
--        finalState
--
--        playerSpawned = any (default false # onMatch { playerSpawn: \s -> s.id == (EntityId "bob") }) evs
--      assertEqual
--        { expected: Nil
--        , actual: newState.pendingSpawns
--        }
--      assertEqual
--        { expected: true
--        , actual: playerSpawned
--        }
