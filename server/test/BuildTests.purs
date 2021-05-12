module Test.BuildTests where

import Prelude
import Blatus.BuildMenu (turret)
import Blatus.Entities (EntityClass(..))
import Blatus.Entities.Tank as Tank
import Blatus.Main as Game
import Blatus.Types (BuildTemplate(..), EntityCommand, GameEntity, GameEvent, RegisteredPlayer, build, buildRequested)
import Control.Monad.Free (Free)
import Data.Maybe (fromJust, isJust, isNothing)
import Data.Tuple (Tuple(..), fst)
import Data.Variant (Variant)
import Debug (spy)
import Erl.Test.EUnit (TestF, suite, test)
import Sisy.Math (Point, origin)
import Sisy.Runtime.Entity (EntityId(..), Cmd)
import Sisy.Runtime.Scene (entityById, updateEntity)
import Sisy.Runtime.Scene as Scene
import Test.Assert (assertFalse, assertTrue)
import Test.Support (entityExists, eventExists, runWhileEvents)

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

sceneWithPlayer :: Scene.Game EntityCommand GameEvent GameEntity
sceneWithPlayer = Scene.addEntity (Tank.init bob origin) $ Scene.initialModel

gameWithPlayerAt :: Point -> Game.State
gameWithPlayerAt location =
  let
    game =
      runWhileEvents
        $ Game.doTick
        $ Game.addPlayer bob
        $ Game.init 0.0
  in
    game { scene = updateEntity (\entity -> entity { location = location }) bob game.scene }

validBuildCommand :: Variant (Cmd EntityCommand)
validBuildCommand =
  ( build
      { location: { x: 0.0, y: 0.0 }
      , template: BuildTemplate "Turret"
      }
  )

tests :: Free TestF Unit
tests = do
  suite "Build menu tests" do
    let
      action = turret
    test "With enough rock, a turret can be built" do
      assertTrue $ action.available (emptyPlayer { availableRock = 60 }) sceneWithPlayer
    test "With no rock, a turret can't be built" do
      assertFalse $ action.available (emptyPlayer { availableRock = 0 }) sceneWithPlayer
    test "Can't build too far away from the player" do
      assertFalse $ isJust $ action.get { x: 250.0, y: 0.0 } emptyPlayer sceneWithPlayer
    test "Can build near the player" do
      assertTrue $ isJust $ action.get { x: 100.0, y: 0.0 } emptyPlayer sceneWithPlayer
    test "Can't build if player doesn't exist" do
      assertFalse $ isJust $ action.get { x: 100.0, y: 0.0 } (emptyPlayer { id = alice }) sceneWithPlayer
  suite "Game build commands" do
    test "Sending a valid build command results in an event being raised" do
      let
        Tuple newGame evs = Game.sendCommand bob validBuildCommand $ gameWithPlayerAt { x: 100.0, y: 0.0 }
      assertTrue $ eventExists { buildRequested: \_ -> true } evs
  test "Raising a valid event results in the entity being built" do
    let
      Tuple newGame evs =
        (flip Game.handleEvent)
          ( buildRequested
              { entity: bob
              , location: { x: 0.0, y: 0.0 }
              , id: EntityId "nice"
              , template: BuildTemplate "turret"
              }
          )
          $ gameWithPlayerAt { x: 100.0, y: 0.0 }
    assertTrue $ entityExists (\e -> e.class == Turret { owner: bob }) newGame.scene
  test "Raising an invalid event results in nothing happening" do
    let
      Tuple newGame evs =
        (flip Game.handleEvent)
          ( buildRequested
              { entity: bob
              , location: { x: 0.0, y: 0.0 }
              , id: EntityId "nice"
              , template: BuildTemplate "blah" -- TODO: Maybe this should be a symbol, with a check to see if exists in the union or some shit
              }
          )
          $ gameWithPlayerAt { x: 100.0, y: 0.0 }
    assertFalse $ entityExists (\e -> e.class == Turret { owner: bob }) newGame.scene
