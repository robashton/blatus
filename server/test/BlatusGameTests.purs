module Test.BlatusGameTests where

import Prelude
import Blatus.Entities.Behaviours.Farmable as Farmable
import Blatus.Entities (CollectableType(..), EntityClass(..))
import Blatus.Main as Main
import Blatus.Types (GameEvent, emptyEntity)
import Sisy.BuiltIn (damage)
import Control.Monad.Free (Free)
import Data.Filterable (filterMap)
import Data.Foldable (foldl, null)
import Data.List (List(..), head, (:))
import Data.Map as Map
import Data.Maybe (Maybe(..), isJust)
import Data.Tuple (Tuple(..), fst, snd, uncurry)
import Data.Variant (Variant, default, onMatch)
import Erl.Test.EUnit (TestF, suite, test)
import Sisy.BuiltIn.Extensions.Collider (entityCollided)
import Sisy.Math (origin)
import Sisy.Runtime.Entity (EntityId(..))
import Sisy.Runtime.Scene (entityById)
import Sisy.Runtime.Scene as Scene
import Test.Assert (assertEqual, assertFalse', assertTrue')
import Test.Support (eventExists, runWhileEvents)

bob :: EntityId
bob = EntityId "bob"

rock :: EntityId
rock = EntityId "rock"


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
          playerSpawned = eventExists { playerSpawn: \s -> s.id == bob } evs
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
                , class: Collectable { width: 10.0, height: 10.0, lifetime: 30, collectableType: Rock 100 }
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

          finalState = runWhileEvents $ Tuple newState evs
        test "Collectable destruction event raised" do
          let
            collectableDestroyed = eventExists { entityDestroyed: \ev -> ev.entity == rock } evs
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
        test "Collectable removed from scene" do
          let
            collectable = entityById rock $ finalState.scene
          assertEqual { expected: false, actual: isJust $ collectable }
        test "Collectable added to player total" do
          let
            playerRock = _.availableRock <$> Map.lookup bob finalState.players
          assertEqual { expected: Just 100, actual: playerRock }
    suite "Farmable resource" do
      let
        entity =
          (emptyEntity rock (Asteroid { width: 25.0, height: 25.0 }))
            { location = { x: 1.0, y: 1.0 }
            , behaviour =
              ( Farmable.init
                  { dropEvery: 25.0
                  , drop:
                      { width: 5.0
                      , height: 5.0
                      , lifetime: 30
                      , collectableType: Rock 10
                      }
                  }
              )
                : Nil
            }

        initialScene = Scene.addEntity entity Scene.initialModel
      test "Damaged but not enough" do
        let
          Tuple newGame evs = Scene.sendCommand rock (damage { amount: 12.5, location: origin, source: Nothing }) initialScene

          dropped = eventExists { collectableSpawned: \ev -> true } evs
        assertFalse' "Item shouldn't drop" dropped
      test "Damaged enough to drop, item drops" do
        let
          Tuple newGame evs = Scene.sendCommand rock (damage { amount: 26.0, location: origin, source: Nothing }) initialScene

          dropped = eventExists { collectableSpawned: \ev -> true } evs
        assertTrue' "Item should drop" dropped
      test "Damaged accumulatively enough to drop, item drops" do
        let
          Tuple newGame evs =
            Scene.sendCommand rock (damage { amount: 13.0, location: origin, source: Nothing })
              $ fst
              $ Scene.sendCommand rock (damage { amount: 13.0, location: origin, source: Nothing }) initialScene

          dropped = eventExists { collectableSpawned: \ev -> true } evs
        assertTrue' "Item should drop" dropped
        pure unit
