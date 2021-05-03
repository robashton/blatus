module Test.RunningGameTests where

import Prelude
import Blatus.Api (RunningGame)
import Blatus.Server.RunningGame as RunningGame
import Blatus.Server.RunningGameList as RunningGameList
import Blatus.Server.RunningGameSup as RunningGameSup
import Blatus.Types (EntityClass(..))
import Control.Monad.Free (Free)
import Data.Array (head, length)
import Data.Either (fromRight)
import Data.Filterable (filter)
import Data.Int (toNumber)
import Data.List (List(..), (:))
import Data.Map as Map
import Data.Maybe (Maybe(..), isJust)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Erl.Data.Tuple (snd)
import Erl.Test.EUnit (TestF, setupTeardown, suite, test)
import Simple.JSON (readJSON)
import Test.Assert (assertEqual, assertTrue')
import Test.Support.App as App
import Test.Support.Requests as Requests

tests :: Free TestF Unit
tests = do
  setupTeardown App.startApp App.stopApp do
    suite "Running ame tests" do
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
          , actual: Map.size $ filter (\e -> e.class == Asteroid) $ state.scene.entities
          }

defaultGame :: RunningGame
defaultGame = { id: "foo", name: "bar", height: 250, width: 250, numPlayers: 2, startedBy: "", public: true, density: 0.05 }
