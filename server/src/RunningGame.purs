module Pure.RunningGame where

import Prelude

import Erl.Atom (atom)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), uncurry)
import Data.Map as Map
import Data.List (toUnfoldable)
import Data.Foldable (foldM)
import Data.Newtype (wrap, unwrap)
import Effect (Effect)
import Pinto (ServerName(..), StartLinkResult)
import Pinto.Gen (CallResult(..), CastResult(..))
import Pinto.Gen as Gen
import Pinto.Timer as Timer
import Pure.Api (RunningGame)
import Pure.Logging as Log
import Pure.Game (Game)
import Pure.Entity (EntityId)
import Pure.Game as Game
import Pure.Comms (ClientMsg(..), GameSync, ServerMsg(..))
import Pure.Comms as Comms
import Effect.Random as Random
import SimpleBus as Bus

type State = { info :: RunningGame
             , game :: Game
             , players :: Map.Map EntityId RegisteredPlayer
             , lastTick :: Int
             }

type RegisteredPlayer = { id :: EntityId
                        , score :: Int
                        , lastTick :: Int
                        }

data Msg = Tick
         | Maintenance
         | DoSync

type StartArgs = { game :: RunningGame }


bus :: String -> Bus.Bus String ServerMsg
bus game = Bus.bus $ "game-" <> game

serverName :: String -> ServerName State Msg
serverName id = Local $ atom $ "runninggame-" <> id

sendCommand :: String -> String -> ClientMsg -> Effect (Maybe ServerMsg)
sendCommand id playerId msg = Gen.call (serverName id) \s@{ game, lastTick, info, players } -> do
  case msg of
    ClientCommand entityCommand -> Gen.lift do
      Bus.raise (bus id) $ ServerCommand { cmd: entityCommand, id: wrap (playerId) }
      let result@(Tuple _ evs) = Game.sendCommand (wrap playerId) entityCommand game
      Bus.raise (bus info.id) $ Comms.ServerEvents $ toUnfoldable evs
      pure $ CallReply Nothing $ s { game = uncurry Game.foldEvents result }
    Ping tick  -> do
      pure $ CallReply (Just $ Pong lastTick) $ s { players = Map.update (\v -> Just $ v { lastTick = tick }) (wrap playerId) players }

-- TODO: EntityId pls
addPlayer :: String -> String -> Effect Unit
addPlayer id playerId = Gen.call (serverName id) \s -> do
  ns <- Gen.lift $ addPlayerToGame playerId s
  pure $ CallReply unit ns


startLink :: StartArgs -> Effect StartLinkResult
startLink args =
  Gen.buildStartLink (serverName args.game.id) (init args) $ Gen.defaultStartLink { handleInfo = handleInfo }

init :: StartArgs -> Gen.Init State Msg
init { game } = do
  Gen.lift $ Log.info Log.RunningGame "Started game" game
  self <- Gen.self
  _ <- Gen.lift $ Timer.sendAfter 0 Tick self
  _ <- Gen.lift $ Timer.sendAfter 1000 DoSync self
  _ <- Gen.lift $ Timer.sendAfter 10000 Maintenance self
  Gen.lift do
    pure $ { info: game
           , game: emptyGame
           , lastTick: 0
           , players: Map.empty
           }

handleInfo :: Msg -> State -> Gen.HandleInfo State Msg
handleInfo msg state@{ info, game, lastTick } = do
  self <- Gen.self
  CastNoReply <$> case msg of
                     Tick -> Gen.lift do
                           newState <- doTick state
                           _ <- Timer.sendAfter 33 Tick self
                           pure $ newState 
                     Maintenance -> Gen.lift do
                           newState <- doMaintenance state
                           _ <- Timer.sendAfter 10000 Maintenance self
                           pure newState
                     DoSync -> Gen.lift do
                           doSync state
                           _ <- Timer.sendAfter 1000 DoSync self
                           pure state


emptyGame :: Game
emptyGame = Game.initialModel

addPlayerToGame :: String -> State -> Effect State
addPlayerToGame playerId s@{ info, game, players } = do
  if
    Map.member (wrap playerId) players then pure s
  else do
    x <- Random.randomInt 0 1000
    y <- Random.randomInt 0 1000
    let player = Game.tank (wrap playerId) { x: Int.toNumber $ x - 500, y: Int.toNumber $ y - 500 }
        newGame = Game.addEntity player game
    Bus.raise  (bus info.id) (NewEntity $ Comms.entityToSync player)
    pure $ s { game = newGame, players = Map.insert (wrap playerId) { id: (wrap playerId), lastTick: s.lastTick, score: 0 } s.players  }


doTick :: State -> Effect State
doTick state@{ game, info, lastTick } = do
  let result@(Tuple _ evs) = Game.tick game
  _ <- Bus.raise (bus info.id) $ Comms.ServerEvents $ toUnfoldable evs
  pure $ state { game = uncurry Game.foldEvents result, lastTick = lastTick + 1 }

playerTimeout :: Int
playerTimeout = 300 -- 10 seconds give or take

doMaintenance :: State -> Effect State
doMaintenance state = do
  foldM (\acc p -> 
     if state.lastTick - p.lastTick > playerTimeout then do
       Bus.raise (bus state.info.id) $ EntityDeleted p.id
       pure $ acc { players = Map.delete p.id acc.players
                  , game = Game.removeEntity p.id acc.game
                  }
     else
       pure acc
    ) state state.players


doSync :: State -> Effect Unit
doSync { info, players, game, lastTick }  = do
  Bus.raise (bus info.id) $ Comms.Sync $ Comms.gameToSync game lastTick
  Bus.raise (bus info.id) $ Comms.UpdatePlayerList $ toUnfoldable $ map (\p -> { 
              playerId: unwrap p.id
            , score: p.score
            , lastTick: p.lastTick
            }) $ Map.values players

