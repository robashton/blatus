module Test.BuiltInTests where

import Prelude
import Control.Monad.Free (Free)
import Data.List (List(..), (:))
import Data.Tuple (Tuple(..), fst)
import Erl.Test.EUnit (TestF, suite, test)
import Sisy.BuiltIn.Behaviours.DisappearsAfter as DisappearsAfter
import Sisy.Runtime.Entity (EntityId(..), emptyEntity)
import Sisy.Runtime.Scene (addEntity)
import Sisy.Runtime.Scene as Scene
import Test.Assert (assertEqual)
import Test.Support (eventExists)

tests :: Free TestF Unit
tests = do
  suite "Built In Behaviours" do
    suite "DisappearsAfter" do
      let
        initialScene = addEntity ((emptyEntity (EntityId "foo") {}) { behaviour = (DisappearsAfter.init 5) : Nil }) $ Scene.initialModel
      test "Doesn't disappear prematurerly" do
        let
          Tuple newScene evs = Scene.tick initialScene
        assertEqual
          { expected: false
          , actual: eventExists { entityDestroyed: \_ -> true } evs
          }
      test "Disappears after 'x' ticks" do
        let
          Tuple newScene evs =
            (Scene.tick <<< fst)
              $ (Scene.tick <<< fst)
              $ (Scene.tick <<< fst)
              $ (Scene.tick <<< fst)
              $ (Scene.tick <<< fst)
              $ Scene.tick initialScene
        assertEqual
          { expected: true
          , actual: eventExists { entityDestroyed: \_ -> true } evs
          }
