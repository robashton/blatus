module Pure.RunningGame where

import Prelude

import Erl.Process.Raw (Pid)
import Erl.Process ((!))
import Erl.Atom (atom)
import Erl.Data.Tuple (tuple2, tuple3)
import Data.Int as Int
import Data.Either (Either(..))
import Erl.Data.Binary (Binary(..))
import Data.Maybe (Maybe(..))
import Erl.Data.Map as Map
import Erl.Process (Process)
import Data.Traversable (traverse)
import Data.Newtype (wrap, unwrap)
import Effect (Effect)
import Erl.Data.List (List)
import Pinto as Pinto
import Pinto (ServerName(..), StartLinkResult)
import Pinto.Gen (CallResult(..), CastResult(..))
import Pinto.Gen as Gen
import Pinto.Timer as Timer
import Pinto.Monitor as Monitor
import Pure.Api (RunningGame)
import Pure.Logging as Log
import Pure.Game (Game)
import Pure.Game as Game
import Effect.Random as Random

type State = { info :: RunningGame, game :: Game  }
data Msg = Tick

type StartArgs = { game :: RunningGame }

serverName :: String -> ServerName State Msg
serverName id = Local $ atom $ "runninggame-" <> id

currentState :: String -> Effect Game
currentState id = Gen.call (serverName id) \s ->
   pure $ CallReply s.game s


addPlayer :: String -> String -> Effect Unit
addPlayer id playerId = Gen.call (serverName id) \s -> do
  ns <- Gen.lift $ addPlayerToGame playerId s
  pure $ CallReply unit s


startLink :: StartArgs -> Effect StartLinkResult
startLink args =
  Gen.buildStartLink (serverName args.game.id) (init args) $ Gen.defaultStartLink { handleInfo = handleInfo }

init :: StartArgs -> Gen.Init State Msg
init { game } = do
  Gen.lift $ Log.info Log.RunningGame "Started game" game
  self <- Gen.self
  Gen.lift do
    pure $ { info: game, game: emptyGame }

handleInfo :: Msg -> State -> Gen.HandleInfo State Msg
handleInfo msg state@{ game } = do
  case msg of
     Tick -> do
        self <- Gen.self
        Gen.lift do
          pure $ CastNoReply state 

emptyGame :: Game
emptyGame = Game.initialModel

addPlayerToGame :: String -> State -> Effect State
addPlayerToGame playerId s@{ game } = do
  x <- Random.randomInt 0 1000
  y <- Random.randomInt 0 1000
  let player = Game.tank (wrap playerId) { x: Int.toNumber $ x - 500, y: Int.toNumber $ y - 500 }
      newGame = Game.addEntity player game
  -- Raise bus message
  pure $ s { game = newGame }








