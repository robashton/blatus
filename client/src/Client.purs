module Blatus.Client where

import Prelude
import Blatus.Client.Input as Input
import Blatus.Client.BuildMenu as BuildMenu
import Blatus.Client.Rendering as Rendering
import Blatus.Client.Ui as Ui
import Blatus.Comms (ClientMsg(..), ServerMsg(..))
import Blatus.Main as Main
import Blatus.Types (EntityCommand)
import Control.Monad.Except (runExcept)
import Data.Either (either)
import Data.Foldable (foldl, for_)
import Data.Maybe (Maybe(..))
import Data.Tuple (fst)
import Data.Variant (Variant, expand)
import Debug (spy)
import Effect (Effect)
import Foreign (readString)
import Signal (Signal, foldp, runSignal, sampleOn)
import Signal as Signal
import Signal.Channel (Channel)
import Signal.Channel as Channel
import Signal.Effect (foldEffect)
import Signal.Time (every, second)
import Signal.Time as Time
import Simple.JSON (readJSON, writeJSON)
import Sisy.Runtime.Entity (EntityId)
import Web.DOM.ParentNode (QuerySelector(..))
import Web.Event.EventTarget as EET
import Web.HTML as HTML
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Location as Location
import Web.HTML.Window as Window
import Web.Socket.Event.EventTypes as WSET
import Web.Socket.Event.MessageEvent as ME
import Web.Socket.ReadyState as RS
import Web.Socket.WebSocket as WS

quitSelector :: QuerySelector
quitSelector = QuerySelector ("#quit")

data GameLoopMsg
  = Input (Variant EntityCommand)
  | GameTick Number
  | ServerMsg ServerMsg

data State
  = Initialising CommonState
  | WaitForFirstSync CommonState GameInfo
  | Running RunningState
  | Error String

type CommonState
  = { socket :: WS.WebSocket
    }

type RunningState
  = { info :: GameInfo
    , gameStateSignal :: Signal GameState
    , serverMessageChannel :: Channel ServerMsg
    , errorMessageChannel :: Channel (Maybe String)
    , common :: CommonState
    }

type GameState
  = { game :: Main.State
    , info :: GameInfo
    , serverTick :: Int
    , tickLatency :: Int
    , now :: Number
    }

tickSignal :: Signal Number
tickSignal = (every $ second / 30.0)

healthCheckSignal :: Signal Number
healthCheckSignal = (every $ second)

pingSignal :: Signal Unit
pingSignal = sampleOn (every $ second) $ Signal.constant unit

main :: Effect Unit
main = do
  window <- HTML.window
  document <- HTMLDocument.toDocument <$> Window.document window
  location <- Window.location window
  socketChannel <- Channel.channel $ ""
  host <- Location.host location
  socket <- createSocket ("ws://" <> host <> "/messaging") $ Channel.send socketChannel
  let
    socketSignal = Channel.subscribe socketChannel
  void $ foldEffect messageLoop (Initialising { socket }) socketSignal

messageLoop :: String -> State -> Effect State
messageLoop "" state = pure state -- that icky first message

messageLoop msg state = do
  case state of
    Initialising common -> either handleServerError (processWelcomeMessage common) $ readJSON msg
    WaitForFirstSync common info -> either handleServerError (waitForFirstSync common info) $ readJSON msg
    Running running -> either handleServerError (proxyServerMessage running) $ readJSON msg
    other -> pure other

processWelcomeMessage :: CommonState -> GameInfo -> Effect State
processWelcomeMessage common info = pure $ WaitForFirstSync common info

waitForFirstSync :: CommonState -> GameInfo -> ServerMsg -> Effect State
waitForFirstSync common info msg = case msg of
  Sync sync -> do
    now <- Time.now
    inputSignal <- Input.signal
    serverMessageChannel <- Channel.channel msg
    errorMessageChannel <- Channel.channel Nothing
    let
      initialGameState =
        { info
        , serverTick: sync.tick
        , tickLatency: 0
        , game: (Main.fromSync now sync)
        , now
        }

      gameStateSignal =
        foldp gameLoop initialGameState
          $ (GameTick <$> tickSignal)
          <> (Input <$> inputSignal)
          <> (ServerMsg <$> Channel.subscribe serverMessageChannel)
    runSignal $ (\gameState -> safeSend common.socket $ writeJSON $ Ping gameState.game.lastTick) <$> sampleOn pingSignal gameStateSignal
    runSignal $ (\cmd -> safeSend common.socket $ writeJSON $ ClientCommand cmd) <$> inputSignal
    runSignal
      $ ( \_ -> do
            socketState <- WS.readyState common.socket
            if socketState == RS.Closing || socketState == RS.Closed then
              Channel.send errorMessageChannel $ Just "Connection has been lost, try refreshing or joining a different game"
            else
              pure unit
        )
      <$> (sampleOn healthCheckSignal $ Signal.constant 1)
    Rendering.init info.playerId $ _.game <$> gameStateSignal
    void $ BuildMenu.init info.playerId $ _.game <$> gameStateSignal
    Ui.init
      { playerId: info.playerId
      , gameUrl: info.gameUrl
      }
      $ ( \g err ->
            { game: g.game
            , tickLatency: g.tickLatency
            , error: err
            }
        )
      <$> gameStateSignal
      <*> Channel.subscribe errorMessageChannel
    pure
      $ Running
          { info
          , gameStateSignal
          , serverMessageChannel
          , errorMessageChannel
          , common
          }
  _ -> pure $ WaitForFirstSync common info

proxyServerMessage :: RunningState -> ServerMsg -> Effect State
proxyServerMessage state@{ serverMessageChannel } msg = do
  void $ Channel.send serverMessageChannel msg
  pure $ Running state

gameLoop :: GameLoopMsg -> GameState -> GameState
gameLoop msg state = case msg of
  Input i -> handleClientCommand i state
  GameTick tick -> handleTick tick state
  ServerMsg serverMsg -> handleServerMessage serverMsg state

handleServerMessage :: ServerMsg -> GameState -> GameState
handleServerMessage msg state@{ info, game } = case msg of
  Sync gameSync -> do
    let
      updated = Main.mergeSyncInfo game gameSync
    state { game = updated, serverTick = gameSync.tick }
  PlayerSync sync -> state { game = Main.mergePlayerSync game sync }
  ServerCommand { id, cmd: cmd } ->
    if id == info.playerId then
      state
    else
      state { game = fst $ Main.sendCommand id (expand cmd) game }
  ServerEvents evs -> state { game = foldl (\a i -> fst $ Main.handleEvent a i) game evs }
  PlayerAdded id -> state { game = Main.addPlayer id game }
  PlayerRemoved id -> state { game = Main.removePlayer id game }
  Pong tick -> state { tickLatency = game.lastTick - tick }

handleClientCommand :: Variant EntityCommand -> GameState -> GameState
handleClientCommand msg state@{ info: { playerId }, game } = state { game = fst $ Main.sendCommand playerId (expand msg) game }

handleTick :: Number -> GameState -> GameState
handleTick now state@{ game } = state { game = fst $ Main.tick now game, now = now }

handleServerError :: forall a. a -> Effect State
handleServerError err =
  let
    _ = spy "Arse" err
  in
    pure $ Error "Something went wrong"

type GameInfo
  = { playerId :: EntityId
    , gameUrl :: String
    }

createSocket :: String -> (String -> Effect Unit) -> Effect WS.WebSocket
createSocket url cb = do
  socket <- WS.create url []
  listener <-
    EET.eventListener \ev ->
      for_ (ME.fromEvent ev) \msgEvent ->
        for_ (runExcept $ readString $ ME.data_ msgEvent) cb
  EET.addEventListener WSET.onMessage listener false (WS.toEventTarget socket)
  pure socket

destroySocket :: WS.WebSocket -> Effect Unit
destroySocket socket = do
  _ <- WS.close socket
  pure unit

safeSend :: WS.WebSocket -> String -> Effect Unit
safeSend ws str = do
  state <- WS.readyState ws
  case state of
    RS.Open -> WS.sendString ws str
    _ -> pure unit
