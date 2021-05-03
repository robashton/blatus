module Blatus.Server.RunningGame where

import Prelude
import Blatus.Api (RunningGame)
import Blatus.Comms (ClientMsg(..), ServerMsg(..))
import Blatus.Comms as Comms
import Blatus.Entities.Asteroid as Asteroid
import Blatus.Main as Main
import Blatus.Server.Logging as Log
import Blatus.Server.RunningGameList as RunningGameList
import Blatus.Server.Timing as Timing
import Blatus.Types (GameEvent)
import Data.Foldable (foldM, foldl)
import Data.Int (decimal, floor, hexadecimal, toNumber, toStringAs)
import Data.Int as Int
import Data.List (List(..), range, toUnfoldable)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), fst, snd, uncurry)
import Data.Variant (Variant, expand)
import Effect (Effect)
import Erl.Atom (atom)
import Pinto (RegistryName(..), StartLinkResult)
import Pinto.GenServer (Action(..), InfoFn, InitResult(..), ServerPid, ServerType)
import Pinto.GenServer as Gen
import Pinto.Timer as Timer
import Pinto.Types (RegistryReference(..))
import SimpleBus as Bus
import Sisy.Runtime.Entity (EntityId(..))
import Sisy.Runtime.Scene as Scene

type State
  = { info :: RunningGame
    , game :: Main.State
    , ticksSinceEmpty :: Int
    }

data Msg
  = Tick
  | Maintenance
  | DoSync

type RunningGameType
  = ServerType Unit Unit Msg State

type RunningGamePid
  = ServerPid Unit Unit Msg State

type StartArgs
  = { game :: RunningGame }

bus :: String -> Bus.Bus String ServerMsg
bus game = Bus.bus $ "game-" <> game

serverName :: String -> RegistryName RunningGameType
serverName id = Local $ atom $ "runninggame-" <> id

latestState :: String -> Effect (Main.State)
latestState id =
  Gen.call (ByName $ serverName id) \_from s@{ game } -> do
    pure $ Gen.reply game s

sendCommand :: String -> String -> ClientMsg -> Effect (Maybe ServerMsg)
sendCommand id playerId msg =
  Gen.call (ByName $ serverName id) \_from s@{ game, info } -> do
    case msg of
      ClientCommand entityCommand ->
        Gen.lift do
          Bus.raise (bus id) $ ServerCommand { cmd: entityCommand, id: wrap (playerId) }
          ug <- uncurry (handleEvents id) $ Main.sendCommand (wrap playerId) (expand entityCommand) game
          pure $ Gen.reply Nothing $ s { game = ug }
      Ping tick -> do
        pure $ Gen.reply (Just $ Pong tick) $ s { game = Main.updatePlayerTick (wrap playerId) tick game }
      Quit ->
        Gen.lift do
          Bus.raise (bus info.id) $ PlayerRemoved $ wrap playerId
          pure $ Gen.reply Nothing $ s { game = Main.removePlayer (wrap playerId) game }

handleEvents :: String -> Main.State -> List (Variant GameEvent) -> Effect Main.State
handleEvents id state Nil = pure state

handleEvents id state evs = do
  _ <- Bus.raise (bus id) $ Comms.ServerEvents $ toUnfoldable evs
  uncurry (handleEvents id) $ foldl (\acc ev -> uncurry (\ng nevs -> Tuple ng $ (snd acc) <> nevs) $ Main.handleEvent (fst acc) ev) (Tuple state Nil) evs

addPlayer :: String -> String -> Effect Unit
addPlayer id playerId =
  Gen.call (ByName $ serverName id) \_from s -> do
    ns <- Gen.lift $ addPlayerToGame (wrap playerId) s
    pure $ Gen.reply unit ns

startLink :: StartArgs -> Effect (StartLinkResult RunningGamePid)
startLink args@{ game } = Gen.startLink $ (Gen.defaultSpec init) { name = Just $ serverName args.game.id, handleInfo = Just handleInfo }
  where
  init = do
    Gen.lift $ Log.info Log.RunningGame "Initialising new game" game
    self <- Gen.self
    Gen.lift do
      void $ Timer.sendAfter 0 Tick self
      void $ Timer.sendAfter 1000 DoSync self
      void $ Timer.sendAfter 10000 Maintenance self
      now <- Int.toNumber <$> Timing.currentMs
      let
        arena = populateArena game $ Main.init now
      pure
        $ InitOk
            { info: game
            , game: arena
            , ticksSinceEmpty: 0
            }

handleInfo :: InfoFn Unit Unit Msg State
handleInfo msg state@{ info, game } = do
  self <- Gen.self
  case msg of
    Tick -> do
      result <-
        Gen.return
          <$> if Map.size game.players == 0 then
              Gen.lift $ emptyGameTick state
            else
              Gen.lift $ tickGame state
      _ <- Gen.lift $ Timer.sendAfter 30 Tick self
      pure result
    Maintenance ->
      Gen.lift do
        newState <- doMaintenance state
        case newState of
          Just s -> do
            _ <- Timer.sendAfter 10000 Maintenance self
            pure $ Gen.return s
          Nothing -> pure $ Gen.returnWithAction StopNormal state
    DoSync ->
      Gen.lift do
        doSync state
        _ <- Timer.sendAfter 1000 DoSync self
        pure $ Gen.return state

emptyGameTick :: State -> Effect State
emptyGameTick state = pure $ state { ticksSinceEmpty = state.ticksSinceEmpty + 1 }

tickGame :: State -> Effect State
tickGame state@{ game, info } = do
  now <- Int.toNumber <$> Timing.currentMs
  ng <- uncurry (handleEvents info.id) $ Main.tick now game
  _ <-
    traverse
      ( \e ->
          if e.networkSync then
            Bus.raise (bus info.id) (PlayerSync $ Main.entityToSync e)
          else
            pure unit
      )
      game.scene.entities
  pure $ state { game = ng, ticksSinceEmpty = 0 }

-- todo: limit the numbers where?
addPlayerToGame :: EntityId -> State -> Effect State
addPlayerToGame playerId s@{ info, game: game@{ players } } = do
  if Map.member playerId players then
    pure s
  else do
    let
      newGame = Main.addPlayer playerId game
    Bus.raise (bus info.id) $ PlayerAdded playerId
    pure $ s { game = newGame }

playerTimeout :: Int
playerTimeout = 300 -- 10 seconds give or take

gameTerminationTickCount :: Int
gameTerminationTickCount = 30 * 60

doMaintenance :: State -> Effect (Maybe State)
doMaintenance state
  | state.ticksSinceEmpty > gameTerminationTickCount = do
    RunningGameList.remove state.info.id
    pure Nothing
  | otherwise = do
    Just
      <$> foldM
          ( \acc p ->
              if state.game.lastTick - p.lastTick > playerTimeout then do
                Bus.raise (bus state.info.id) $ PlayerRemoved p.id
                pure $ acc { game = Main.removePlayer p.id acc.game }
              else
                pure acc
          )
          state
          state.game.players

doSync :: State -> Effect Unit
doSync { info, game } = do
  let
    sync = Main.toSync game
  Bus.raise (bus info.id) $ Comms.Sync $ sync

populateArena :: RunningGame -> Main.State -> Main.State
populateArena { width, height, density } state =
  let
    adjustedScene =
      state.scene
        { world =
          { x: (toNumber (-width)) / 2.0
          , y: (toNumber (-height)) / 2.0
          , width: toNumber width
          , height: toNumber height
          }
        }

    numAsteroids = floor $ (density * (toNumber width) * (toNumber height)) / 100.0

    adjustedState =
      state
        { scene = adjustedScene
        }
  in
    foldl (\s i -> spawnAsteroid (asteroidId i) s) adjustedState $ range 0 numAsteroids

asteroidId :: Int -> EntityId
asteroidId x = EntityId $ "asteroid-" <> (toStringAs hexadecimal x)

spawnAsteroid :: EntityId -> Main.State -> Main.State
spawnAsteroid id state@{ seed, scene } =
  let
    (Tuple x s1) = Main.random Tuple seed

    (Tuple y s2) = Main.random Tuple s1

    (Tuple size s3) = Main.random Tuple s2

    asteroid =
      Asteroid.init id
        { x: x * scene.world.width + scene.world.x
        , y: y * scene.world.height + scene.world.y
        }
        (size * 20.0 - 10.0)
        (size * 20.0 - 10.0)
  in
    state
      { seed = s3
      , scene = Scene.addEntity asteroid scene
      }
