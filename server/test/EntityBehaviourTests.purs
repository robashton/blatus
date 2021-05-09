module Test.EntityBehaviourTests where

import Prelude
import Control.Monad.Free (Free)
import Data.Exists (Exists, mkExists)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Data.Variant (default, onMatch)
import Erl.Test.EUnit (TestF, suite, test)
import Sisy.Runtime.Behaviour as B
import Sisy.Runtime.Entity (EntityBehaviour(..), EntityId(..), emptyEntity)
import Sisy.Runtime.Scene (Game, addEntity)
import Sisy.Runtime.Scene as Scene
import Test.Assert (assertEqual)

tests :: Free TestF Unit
tests = do
  suite "Entity behaviours" do
    test "Can modify entity state" do
      let
        initialScene = addEntity ((emptyEntity (EntityId "foo") { wasModified: false }) { behaviour = modifyBehaviour : Nil }) emptyScene

        Tuple newScene evs = Scene.tick initialScene

        updatedEntityState = _.wasModified <$> Scene.entityById (EntityId "foo") newScene
      assertEqual
        { expected: Just true
        , actual: updatedEntityState
        }
    test "Can request other entities state" do
      let
        initialScene =
          addEntity (emptyEntity (EntityId "bar") { someValue: 1337 })
            $ addEntity ((emptyEntity (EntityId "foo") { someValue: 0 }) { behaviour = (copySomeValueFrom (EntityId "bar")) : Nil }) emptyScene

        Tuple newScene evs = Scene.tick initialScene

        updatedEntityState = _.someValue <$> Scene.entityById (EntityId "foo") newScene
      assertEqual
        { expected: Just 1337
        , actual: updatedEntityState
        }

modifyBehaviour :: forall entity. Exists (EntityBehaviour () () ( wasModified :: Boolean | entity ))
modifyBehaviour =
  mkExists
    $ EntityBehaviour
        { state: {}
        , handleCommand:
            ( \cmd state ->
                onMatch
                  { tick:
                      \_ -> do
                        B.updateEntity (\e -> e { wasModified = true })
                        pure state
                  }
                  (default (pure state))
                  cmd
            )
        }

copySomeValueFrom :: forall entity. EntityId -> Exists (EntityBehaviour () () ( someValue :: Int | entity ))
copySomeValueFrom source =
  mkExists
    $ EntityBehaviour
        { state: {}
        , handleCommand:
            ( \cmd state ->
                onMatch
                  { tick:
                      \_ -> do
                        scene <- B.scene
                        let
                          value = _.someValue <$> scene.entityById source
                        B.updateEntity (\e -> e { someValue = fromMaybe 0 value })
                        pure state
                  }
                  (default (pure state))
                  cmd
            )
        }

emptyScene :: forall entity. Game () () entity
emptyScene = Scene.initialModel
