module Test.BuildMenuTests where

import Prelude
import Blatus.BuildMenu (turret)
import Blatus.Entities.Tank as Tank
import Blatus.Types (GameEntity, GameEvent, RegisteredPlayer, EntityCommand)
import Control.Monad.Free (Free)
import Data.Maybe (isJust, isNothing)
import Erl.Test.EUnit (TestF, suite, test)
import Sisy.Math (origin)
import Sisy.Runtime.Entity (EntityId(..))
import Sisy.Runtime.Scene as Scene
import Test.Assert (assertFalse, assertTrue)

bob :: EntityId
bob = EntityId "bob"

alice :: EntityId
alice = EntityId "alice"

emptyPlayer :: RegisteredPlayer
emptyPlayer =
  { id: bob
  , availableRock: 0
  , lastTick: 0
  , score: 0
  }

emptyScene :: Scene.Game EntityCommand GameEvent GameEntity
emptyScene = Scene.addEntity (Tank.init bob origin) $ Scene.initialModel

tests :: Free TestF Unit
tests = do
  suite "Build menu tests" do
    let
      action = turret
    test "With enough rock, a turret can be built" do
      assertTrue $ action.available (emptyPlayer { availableRock = 60 }) emptyScene
    test "With no rock, a turret can't be built" do
      assertFalse $ action.available (emptyPlayer { availableRock = 0 }) emptyScene
    test "Can't build too far away from the player" do
      assertFalse $ isJust $ action.get { x: 250.0, y: 0.0 } emptyPlayer emptyScene
    test "Can build near the player" do
      assertTrue $ isJust $ action.get { x: 100.0, y: 0.0 } emptyPlayer emptyScene
    test "Can't build if player doesn't exist" do
      assertFalse $ isJust $ action.get { x: 100.0, y: 0.0 } (emptyPlayer { id = alice }) emptyScene
