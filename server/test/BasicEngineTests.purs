module Test.BasicEngineTests where

import Prelude
import Control.Monad.Free (Free)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Erl.Test.EUnit (TestF, suite, test)
import Pure.Entity (EntityId(..), emptyEntity)
import Pure.Runtime.Scene (Game, addEntity)
import Pure.Runtime.Scene as Scene
import Test.Assert (assert, assertEqual)
import Test.Support.Types (TestCommand(..), TestEvent(..), tickEcho)

tests :: Free TestF Unit
tests = do
  suite "Basic engine stuff" do
    test "Entities tick when scene ticks" do
      let
        initialScene = addEntity ((emptyEntity (EntityId "foo") {}) { behaviour = tickEcho : Nil }) emptyScene

        Tuple newScene evs = Scene.tick initialScene
      assertEqual
        { expected: (Ticked (EntityId "foo")) : Nil
        , actual: evs
        }
    test "Sending a command directly to a single entity" do
      let
        initialScene :: Game TestCommand TestEvent ()
        initialScene =
          addEntity ((emptyEntity (EntityId "foo") {}) { behaviour = tickEcho : Nil })
            $ addEntity ((emptyEntity (EntityId "bar") {}) { behaviour = tickEcho : Nil }) emptyScene

        Tuple newScene evs = Scene.sendCommand (EntityId "foo") Tick initialScene
      assertEqual
        { expected: (Ticked (EntityId "foo")) : Nil
        , actual: evs
        }
    test "Fetching an entity by Id" do
      let
        initialScene =
          addEntity ((emptyEntity (EntityId "foo") {}) { behaviour = tickEcho : Nil })
            $ addEntity ((emptyEntity (EntityId "bar") {}) { behaviour = tickEcho : Nil }) emptyScene

        entityId = _.id <$> Scene.entityById (EntityId "foo") initialScene
      assertEqual
        { expected: Just (EntityId "foo")
        , actual: entityId
        }
    test "Removing an entity" do
      let
        initialScene =
          addEntity ((emptyEntity (EntityId "foo") {}) { behaviour = tickEcho : Nil })
            $ addEntity ((emptyEntity (EntityId "bar") {}) { behaviour = tickEcho : Nil }) emptyScene

        newScene = Scene.removeEntity (EntityId "foo") initialScene

        entity = _.id <$> Scene.entityById (EntityId "foo") newScene
      assertEqual
        { expected: Nothing
        , actual: entity
        }

emptyScene :: Game TestCommand TestEvent ()
emptyScene = Scene.initialModel Tick
