module Test.BasicAppTests where

import Prelude
import Blatus.Api (RunningGame)
import Blatus.Server.RunningGameList as RunningGameList
import Control.Monad.Free (Free)
import Data.Array (head, length)
import Data.Either (fromRight)
import Data.List (List(..), (:))
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
    suite "Basic app stuff" do
      test "Can at least request the bloody home page" do
        page <- snd <$> Requests.requestPage "/"
        assertTrue' "Was able to request the bloody home page" (isJust page)
    suite "Game Creation API" do
      test "Creating a public game via the API" do
        createGameViaApi true gameName
        game <- RunningGameList.findByName gameName
        assertEqual
          { expected: Just playerName
          , actual: _.startedBy <$> game
          }
        assertEqual
          { expected: Just true
          , actual: _.public <$> game
          }
      test "Creating a private game via the API" do
        createGameViaApi false gameName
        game <- RunningGameList.findByName gameName
        assertEqual
          { expected: Just playerName
          , actual: _.startedBy <$> game
          }
        assertEqual
          { expected: Just false
          , actual: _.public <$> game
          }
      test "Listing games via the API" do
        createGameViaApi false gameName
        createGameViaApi true gameName2
        json <- snd <$> Requests.requestPage "/games"
        let
          games :: Maybe (Array RunningGame)
          games = fromRight [] <$> readJSON <$> json
        assertEqual
          { expected: Just gameName2
          , actual: _.name <$> (head =<< games)
          }
        assertEqual
          { expected: Just 1
          , actual: length <$> games
          }

playerName :: String
playerName = "bob"

gameName :: String
gameName = "flibble"

gameName2 :: String
gameName2 = "flobble"

createGameViaApi :: Boolean -> String -> Effect Unit
createGameViaApi public n = void $ Requests.postForm "/games" (Tuple "player-name" "bob" : Tuple "game-name" n : Tuple "public" (if public then "on" else "off") : Nil)
