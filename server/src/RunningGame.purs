module Pure.RunningGame where

import Prelude

import Data.Array as Array
import Data.Foldable (foldM, foldl)
import Data.Int as Int
import Data.List (toUnfoldable, List(..))
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap, unwrap)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), fst, snd, uncurry)
import Effect (Effect)
import Erl.Atom (atom)
import Erl.Process (Process(..))
import Fprof as Fprof
import Pinto (ServerName(..), StartLinkResult)
import Pinto.Gen (CallResult(..), CastResult(..))
import Pinto.Gen as Gen
import Pinto.Timer as Timer
import Pinto.Types (TerminateReason(..))
import Pure.Api (RunningGame)
import Pure.Comms (ClientMsg(..), GameSync, ServerMsg(..))
import Pure.Comms as Comms
import Pure.Entities.Tank as Tank
import Pure.Entity (EntityId)
import Pure.Game.Main as Main
import Pure.Logging as Log
import Pure.RunningGameList as RunningGameList
import Pure.Runtime.Scene (Game)
import Pure.Runtime.Scene as Scene
import Pure.Timing as Timing
import Pure.Types (EntityCommand, GameEvent)
import Pure.Types (GameEvent(..))
import SimpleBus as Bus


type State = { info :: RunningGame
             , game :: Main.State
             , ticksSinceEmpty :: Int
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
sendCommand id playerId msg = Gen.call (serverName id) \s@{ game, info } -> do
  case msg of
    ClientCommand entityCommand -> Gen.lift do
      Bus.raise (bus id) $ ServerCommand { cmd: entityCommand, id: wrap (playerId) }
      ug <- uncurry (handleEvents id) $ Main.sendCommand (wrap playerId) entityCommand game
      pure $ CallReply Nothing $ s { game = ug }
    Ping tick  -> do
      pure $ CallReply (Just $ Pong tick) $ s { game = Main.updatePlayerTick (wrap playerId) tick game  }
    Quit -> Gen.lift do
      Bus.raise (bus info.id) $ PlayerRemoved $ wrap playerId
      pure $ CallReply Nothing $ s { game = Main.removePlayer (wrap playerId) game }

handleEvents :: String -> Main.State -> List GameEvent -> Effect Main.State
handleEvents id state Nil = pure state
handleEvents id state evs = do
  _ <- Bus.raise (bus id) $ Comms.ServerEvents $ toUnfoldable evs
  uncurry (handleEvents id) $ foldl (\acc ev -> uncurry (\ng nevs -> Tuple ng $ (snd acc) <> nevs ) $ Main.handleEvent (fst acc) ev) (Tuple state Nil) evs

addPlayer :: String -> String -> Effect Unit
addPlayer id playerId = Gen.call (serverName id) \s -> do
  ns <- Gen.lift $ addPlayerToGame (wrap playerId) s
  pure $ CallReply unit ns


startLink :: StartArgs -> Effect StartLinkResult
startLink args =
  Gen.buildStartLink (serverName args.game.id) (init args) $ Gen.defaultStartLink { handleInfo = handleInfo }

init :: StartArgs -> Gen.Init State Msg
init { game } = do
  self <- Gen.self
  Gen.lift do
    --Fprof.start
    Log.info Log.RunningGame "Started game" game
    void $ Timer.sendAfter 0 Tick self
    void $ Timer.sendAfter 1000 DoSync self
    void $ Timer.sendAfter 10000 Maintenance self
    now <- Int.toNumber <$> Timing.currentMs
    pure $ { info: game
           , game: Main.init now
           , ticksSinceEmpty: 0
           }

handleInfo :: Msg -> State -> Gen.HandleInfo State Msg
handleInfo msg state@{ info, game } = do
  self <- Gen.self
  case msg of
       Tick -> do
         result <- CastNoReply <$> if Map.size game.players == 0 then 
                                     Gen.lift $ emptyGameTick state
                                       else 
                                     Gen.lift $ tickGame state 
         _ <- Gen.lift $ Timer.sendAfter 30 Tick self
         pure result

       Maintenance -> Gen.lift do
          newState <- doMaintenance state
          case newState of
             Just s -> do
               _ <- Timer.sendAfter 10000 Maintenance self
               pure $ CastNoReply s

             Nothing ->
               pure $ CastStopReason Normal state

       DoSync -> Gen.lift do
          doSync state
          _ <- Timer.sendAfter 1000 DoSync self
          pure $ CastNoReply state

emptyGameTick :: State -> Effect State
emptyGameTick state = pure $ state { ticksSinceEmpty = state.ticksSinceEmpty + 1 }

tickGame :: State -> Effect State
tickGame state@{ game, info } = do
  now <- Int.toNumber <$> Timing.currentMs
  ng <- uncurry (handleEvents info.id) $ Main.tick now game
  _ <- traverse (\e -> if e.networkSync then
                  Bus.raise  (bus info.id) (PlayerSync $ Main.entityToSync e)
                else
                pure unit
                ) game.scene.entities
  pure $ state { game = ng, ticksSinceEmpty = 0}  

addPlayerToGame :: EntityId -> State -> Effect State
addPlayerToGame playerId s@{ info, game: game@{ players } } = do
  if
    Map.member  playerId players then pure s
  else do
    let newGame = Main.addPlayer playerId game
    Bus.raise  (bus info.id) $ PlayerAdded  playerId
    pure $ s { game = newGame }

playerTimeout :: Int
playerTimeout = 300 -- 10 seconds give or take

gameTerminationTickCount :: Int
gameTerminationTickCount = 30 * 60

doMaintenance :: State -> Effect (Maybe State)
doMaintenance state 
  | state.ticksSinceEmpty > gameTerminationTickCount =  do
    RunningGameList.remove state.info.id
    pure Nothing
  | otherwise = do
    Just <$> foldM (\acc p -> 
       if state.game.lastTick - p.lastTick > playerTimeout then do
         Bus.raise (bus state.info.id) $ PlayerRemoved p.id
         pure $ acc { game = Main.removePlayer p.id acc.game }
       else
         pure acc
      ) state state.game.players


doSync :: State -> Effect Unit
doSync { info, game }  = do
  let sync = Main.toSync game
  Bus.raise (bus info.id) $ Comms.Sync $ sync
