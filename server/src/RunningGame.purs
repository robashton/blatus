module Pure.RunningGame where

import Prelude

import Data.Array as Array
import Data.Foldable (foldM, foldl)
import Data.Int as Int
import Data.List (toUnfoldable)
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap, unwrap)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), uncurry)
import Effect (Effect)
import Effect.Random as Random
import Erl.Atom (atom)
import Fprof as Fprof
import Pinto (ServerName(..), StartLinkResult)
import Pinto.Gen (CallResult(..), CastResult(..))
import Pinto.Gen as Gen
import Pinto.Timer as Timer
import Pure.Api (RunningGame)
import Pure.Comms (ClientMsg(..), GameSync, ServerMsg(..))
import Pure.Comms as Comms
import Pure.Entities.Tank as Tank
import Pure.Entity (EntityId)
import Pure.Game.Main as Main
import Pure.Logging as Log
import Pure.Runtime.Scene (Game)
import Pure.Runtime.Scene as Scene
import Pure.Types (GameEvent(..))
import Pure.Ticks as Ticks
import Pure.Timing as Timing
import Pure.Types (EntityCommand, GameEvent)
import SimpleBus as Bus

timePerFrame :: Number
timePerFrame = 1000.0 / 30.0

type State = { info :: RunningGame
             , game :: Main.State
             , players :: Map.Map EntityId RegisteredPlayer
             , lastTick :: Int
             , ticks :: Ticks.State
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
  _ <- Gen.lift $ Log.info Log.RunningGame "Received message" { lastTick, msg }
  case msg of
    ClientCommand entityCommand -> Gen.lift do
      Bus.raise (bus id) $ ServerCommand { cmd: entityCommand, id: wrap (playerId) }
      let result@(Tuple _ evs) = Main.sendCommand (wrap playerId) entityCommand game
      if not List.null evs then Bus.raise (bus info.id) $ Comms.ServerEvents $ toUnfoldable evs
      else pure unit
      pure $ CallReply Nothing $ s { game = uncurry (foldl Main.handleEvent) result }
    Ping tick  -> do
      pure $ CallReply (Just $ Pong tick) $ s { players = Map.update (\v -> Just $ v { lastTick = tick }) (wrap playerId) players }

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
  self <- Gen.self
  Gen.lift do
    --Fprof.start
    Log.info Log.RunningGame "Started game" game
    void $ Timer.sendAfter 0 Tick self
    void $ Timer.sendAfter 1000 DoSync self
    void $ Timer.sendAfter 10000 Maintenance self
    now <- Int.toNumber <$> Timing.currentMs
    pure $ { info: game
           , game: emptyGame
           , lastTick: 0
           , players: Map.empty
           , ticks: Ticks.init now timePerFrame
           }

handleInfo :: Msg -> State -> Gen.HandleInfo State Msg
handleInfo msg state@{ info, game, lastTick } = do
  self <- Gen.self
  CastNoReply <$> case msg of
                     Tick -> Gen.lift do
                           now <- Int.toNumber <$> Timing.currentMs
                           let (Tuple framesToExecute newTicks) = Ticks.update now state.ticks
                           newState <- foldM (\acc x -> if x == 0 then pure acc else doTick acc) state $ Array.range 0 framesToExecute
                           _ <- traverse (\e -> if e.networkSync then
                                                  Bus.raise  (bus info.id) (PlayerSync $ Main.entityToSync e)
                                                else
                                                  pure unit
                                                  ) game.scene.entities

                           _ <- Timer.sendAfter 30 Tick self
                           pure $ newState { ticks = newTicks } 
                     Maintenance -> Gen.lift do
                           newState <- doMaintenance state
                           _ <- Timer.sendAfter 10000 Maintenance self
                           pure newState
                     DoSync -> Gen.lift do
                           doSync state
                           _ <- Timer.sendAfter 1000 DoSync self
                           pure state


emptyGame :: Main.State
emptyGame = Main.init

addPlayerToGame :: String -> State -> Effect State
addPlayerToGame playerId s@{ info, game, players } = do
  if
    Map.member (wrap playerId) players then pure s
  else do
    x <- Random.randomInt 0 1000
    y <- Random.randomInt 0 1000
    let player = Tank.init (wrap playerId) Tank.Server { x: Int.toNumber $ x - 500, y: Int.toNumber $ y - 500 }
        newScene = Scene.addEntity player game.scene
    Bus.raise  (bus info.id) (NewEntity $ Main.entityToSync player)
    pure $ s { game = game { scene = newScene }, players = Map.insert (wrap playerId) { id: (wrap playerId), lastTick: s.lastTick, score: 0 } s.players  }


doTick :: State -> Effect State
doTick state@{ game, info, lastTick } = do
  start <- Timing.currentMs
  let result@(Tuple _ evs) = Main.tick game

  if not List.null evs then Bus.raise (bus info.id) $ Comms.ServerEvents $ toUnfoldable evs
  else pure unit

  end <- Timing.currentMs
  pure $ state { game = uncurry (foldl Main.handleEvent) result, lastTick = lastTick + 1 }


playerTimeout :: Int
playerTimeout = 300 -- 10 seconds give or take

doMaintenance :: State -> Effect State
doMaintenance state = do
  foldM (\acc p -> 
     if state.lastTick - p.lastTick > playerTimeout then do
       Bus.raise (bus state.info.id) $ EntityDeleted p.id
       pure $ acc { players = Map.delete p.id acc.players
                  , game = Main.removeEntity p.id acc.game
                  }
     else
       pure acc
    ) state state.players


doSync :: State -> Effect Unit
doSync { info, players, game, lastTick }  = do
  let sync = Main.toSync game lastTick
  Bus.raise (bus info.id) $ Comms.Sync $ sync
  Bus.raise (bus info.id) $ Comms.UpdatePlayerList $ toUnfoldable $ map (\p -> { 
              playerId: unwrap p.id
            , score: p.score
            , lastTick: p.lastTick
            }) $ Map.values players

