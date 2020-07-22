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
import Pure.Comms (ClientMsg(..), ServerMsg(..))
import Pure.Comms as Comms
import Effect.Random as Random
import SimpleBus as Bus

type State = { info :: RunningGame, game :: Game  }
data Msg = Tick

type StartArgs = { game :: RunningGame }


bus :: String -> Bus.Bus String ServerMsg
bus game = Bus.bus $ "game-" <> game

serverName :: String -> ServerName State Msg
serverName id = Local $ atom $ "runninggame-" <> id

currentState :: String -> Effect Game
currentState id = Gen.call (serverName id) \s ->
   pure $ CallReply s.game s

sendCommand :: String -> String -> ClientMsg -> Effect Unit
sendCommand id playerId msg = Gen.call (serverName id) (\s@{ game } -> do
  case msg of
    ClientCommand entityCommand -> do
      Gen.lift $ Bus.raise (bus id) $ ServerCommand { cmd: entityCommand, id: wrap id }
      pure $ CallReply unit $ s { game = Game.foldEvents $ Game.sendCommand (wrap playerId) entityCommand game }
  )

-- TODO: EntityId pls
addPlayer :: String -> String -> Effect Unit
addPlayer id playerId = Gen.call (serverName id) \s -> do
  ns <- Gen.lift $ addPlayerToGame playerId s
  pure $ CallReply unit ns

-- TODO: we'll do these as sendAfters or something
-- but essentially don't remove the player just cos we've disconnected temporariily
-- give it 10 seconds in case they've just refreshed the browser

-- startPlayerTimeout :: String -> String -> Effect Unit


startLink :: StartArgs -> Effect StartLinkResult
startLink args =
  Gen.buildStartLink (serverName args.game.id) (init args) $ Gen.defaultStartLink { handleInfo = handleInfo }

init :: StartArgs -> Gen.Init State Msg
init { game } = do
  Gen.lift $ Log.info Log.RunningGame "Started game" game
  self <- Gen.self
  _ <- Gen.lift $ Timer.sendAfter 0 Tick self
  Gen.lift do
    pure $ { info: game, game: emptyGame }

handleInfo :: Msg -> State -> Gen.HandleInfo State Msg
handleInfo msg state@{ game } = do
  case msg of
     Tick -> do
        self <- Gen.self
        Gen.lift do
           newGame <- doTick game
           -- TODO: Calculate how long tick took and adjust this based on that
           -- We probably need to do an overall timer for this too
           -- cos 33.333333 means every three frames we'd lose a ms..
           _ <- Timer.sendAfter 33 Tick self
           pure $ CastNoReply $ state { game = newGame }

emptyGame :: Game
emptyGame = Game.initialModel

addPlayerToGame :: String -> State -> Effect State
addPlayerToGame playerId s@{ info, game } = do
  x <- Random.randomInt 0 1000
  y <- Random.randomInt 0 1000
  let player = Game.tank (wrap playerId) { x: Int.toNumber $ x - 500, y: Int.toNumber $ y - 500 }
      newGame = Game.addEntity player game
  Bus.raise  (bus info.id) (NewEntity $ Comms.entityToSync player)
  pure $ s { game = newGame }


doTick :: Game -> Effect Game
doTick game = 
  -- TODO: Send events out on bus for folding into clients
  -- as they ignore their own
  pure $ Game.foldEvents $ Game.tick game

