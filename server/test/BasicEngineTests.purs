module Test.BasicEngineTests where

import Prelude
import Control.Monad.Free (Free)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Erl.Test.EUnit (TestF, suite, test)
import Sisy.Runtime.Entity (EntityId(..), emptyEntity)
import Sisy.Runtime.Scene (Game, addEntity, tickCmd)
import Sisy.Runtime.Scene as Scene
import Test.Assert (assertEqual)
import Test.Support (TestEvent, tickEcho, ticked)

tests :: Free TestF Unit
tests = do
  suite "Basic engine stuff" do
    test "Entities tick when scene ticks" do
      let
        initialScene = addEntity ((emptyEntity (EntityId "foo") {}) { behaviour = tickEcho : Nil }) emptyScene

        Tuple newScene evs = Scene.tick initialScene
      assertEqual
        { expected: (ticked (EntityId "foo")) : Nil
        , actual: evs
        }
    test "Sending a command directly to a single entity" do
      let
        initialScene =
          addEntity ((emptyEntity (EntityId "foo") {}) { behaviour = tickEcho : Nil })
            $ addEntity ((emptyEntity (EntityId "bar") {}) { behaviour = tickEcho : Nil }) emptyScene

        Tuple newScene evs = Scene.sendCommand (EntityId "foo") tickCmd initialScene
      assertEqual
        { expected: (ticked (EntityId "foo")) : Nil
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

emptyScene :: Game () TestEvent ()
emptyScene = Scene.initialModel
