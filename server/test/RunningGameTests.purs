module Test.RunningGameTests where

import Prelude
import Blatus.Api (RunningGame)
import Blatus.Server.RunningGame as RunningGame
import Blatus.Server.RunningGameSup as RunningGameSup
import Blatus.Entities (EntityClass(..))
import Control.Monad.Free (Free)
import Data.Filterable (filter)
import Data.Map as Map
import Erl.Test.EUnit (TestF, setupTeardown, suite, test)
import Test.Assert (assertEqual)
import Test.Support.App as App

tests :: Free TestF Unit
tests = do
  setupTeardown App.startApp App.stopApp do
    suite "Running game tests" do
      test "Asteroid population" do
        let
          args = defaultGame
        -- note: probably need to seed for consistent results
        void $ RunningGameSup.startGame { game: args }
        state <- RunningGame.latestState "foo"
        assertEqual
          { expected: { x: (-125.0), y: (-125.0), width: 250.0, height: 250.0 }
          , actual: state.scene.world
          }
        assertEqual
          { expected: 32 -- bleh
          , actual:
              Map.size
                $ filter
                    ( \e -> case e.class of
                        Asteroid _ -> true
                        _ -> false
                    )
                $ state.scene.entities
          }

defaultGame :: RunningGame
defaultGame = { id: "foo", name: "bar", height: 250, width: 250, numPlayers: 2, startedBy: "", public: true, density: 0.05 }
