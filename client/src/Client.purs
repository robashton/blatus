module Blatus.Client where

import Prelude
import Blatus.Client.Rendering as Rendering
import Blatus.Client.Ui as Ui
import Blatus.Comms (ClientMsg(..), ServerMsg(..))
import Blatus.Entities.Tank as Tank
import Blatus.Main as Main
import Blatus.Types (EntityCommand)
import Control.Apply (lift2)
import Control.Monad.Except (runExcept)
import Data.DateTime.Instant as Instant
import Data.Either (either)
import Data.Foldable (foldl, for_)
import Data.Map (lookup) as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (unwrap, wrap)
import Data.Symbol (SProxy(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (traverse)
import Data.Tuple (fst)
import Data.Variant (Variant, expand, inj)
import Effect (Effect)
import Effect.Now as Now
import Foreign (readString)
import Signal (Signal, dropRepeats, foldp, runSignal, sampleOn)
import Signal as Signal
import Signal.Channel as Channel
import Signal.DOM (keyPressed)
import Signal.Time (every, second)
import Simple.JSON (readJSON, writeJSON)
import Sisy.Runtime.Entity (EntityId(..))
import Sisy.Runtime.Scene (entityById)
import Sisy.Types (empty)
import Web.DOM.Document as Document
import Web.DOM.Element as Element
import Web.DOM.Node as Node
import Web.DOM.NodeList as NodeList
import Web.DOM.ParentNode (QuerySelector(..), querySelector)
import Web.Event.EventTarget as EET
import Web.Event.EventTarget as ET
import Web.HTML as HTML
import Web.HTML.Event.EventTypes as ETS
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Location as Location
import Web.HTML.Window as Window
import Web.Socket.Event.EventTypes as WSET
import Web.Socket.Event.MessageEvent as ME
import Web.Socket.ReadyState as RS
import Web.Socket.WebSocket as WS

type GameInfo
  = { playerId :: EntityId
    , gameUrl :: String
    }

type LocalContext
  = { window :: HTML.Window
    , game :: Main.State
    , socketChannel :: Channel.Channel String
    , socket :: WS.WebSocket
    , serverTick :: Int
    , tickLatency :: Int
    , info :: Maybe GameInfo
    , isStarted :: Boolean
    , hasError :: Boolean
    , now :: Number
    }

quitSelector :: QuerySelector
quitSelector = QuerySelector ("#quit")

rotateLeftSignal :: Effect (Signal (Variant EntityCommand))
rotateLeftSignal = do
  key <- keyPressed 37
  pure $ dropRepeats $ (\x -> if x then (inj (SProxy :: SProxy "turnLeft") empty) else (inj (SProxy :: SProxy "stopTurnLeft") empty)) <$> key

thrustSignal :: Effect (Signal (Variant EntityCommand))
thrustSignal = do
  key <- keyPressed 38
  pure $ dropRepeats $ (\x -> if x then (inj (SProxy :: SProxy "pushForward") empty) else (inj (SProxy :: SProxy "stopPushForward") empty)) <$> key

rotateRightSignal :: Effect (Signal (Variant EntityCommand))
rotateRightSignal = do
  key <- keyPressed 39
  pure $ dropRepeats $ (\x -> if x then (inj (SProxy :: SProxy "turnRight") empty) else (inj (SProxy :: SProxy "stopTurnRight") empty)) <$> key

brakeSignal :: Effect (Signal (Variant EntityCommand))
brakeSignal = do
  key <- keyPressed 40
  pure $ dropRepeats $ (\x -> if x then (inj (SProxy :: SProxy "pushBackward") empty) else (inj (SProxy :: SProxy "stopPushBackward") empty)) <$> key

fireSignal :: Effect (Signal (Variant EntityCommand))
fireSignal = do
  key <- keyPressed 32
  pure $ dropRepeats $ (\x -> if x then (inj (SProxy :: SProxy "startFireBullet") empty) else (inj (SProxy :: SProxy "stopFireBullet") empty)) <$> key

inputSignal :: Effect (Signal (Variant EntityCommand))
inputSignal = do
  fs <- fireSignal
  rl <- rotateLeftSignal
  ts <- thrustSignal
  rr <- rotateRightSignal
  bs <- brakeSignal
  pure $ fs <> rl <> ts <> rr <> bs

data GameLoopMsg
  = Input (Variant EntityCommand)
  | GameTick { time :: Number, hasError :: Boolean }
  | Ws String

tickSignal :: Signal Unit
tickSignal = sampleOn (every $ second / 30.0) $ Signal.constant unit

pingSignal :: Signal Unit
pingSignal = sampleOn (every $ second) $ Signal.constant unit

uiUpdateSignal :: Signal Unit
uiUpdateSignal = sampleOn (every $ second * 0.3) $ Signal.constant unit

load :: (LocalContext -> Effect Unit) -> Effect Unit
load cb = do
  window <- HTML.window
  location <- Window.location window
  socketChannel <- Channel.channel $ ""
  host <- Location.host location
  socket <- createSocket ("ws://" <> host <> "/messaging") $ Channel.send socketChannel
  Milliseconds now <- Instant.unInstant <$> Now.now
--  let
--    game = Main.init now
-- TODO: Dear Rob in the morning
-- info/game just need to be their own thing
-- that only get created once we get the welcome message
-- We can subscribe to that (or just do it) to start the renderer and the build menu system
-- this cb goes away too
  cb
    { window
    , info: Nothing
    , socket
    , socketChannel
    , serverTick: 0
    , now
    , tickLatency: 0
    , isStarted: false
    , hasError: false
    }

main :: Effect Unit
main = do
  load
    ( \loadedContext@{ socket, window } -> do
        gameInput <- inputSignal
        document <- HTMLDocument.toDocument <$> Window.document window
        location <- Window.location window
        Milliseconds start <- Instant.unInstant <$> Now.now
        ticksChannel <- Channel.channel { time: start, hasError: false }
        quitChannel <- Channel.channel false
        gameStartedChannel <- Channel.channel false
        quitElement <- querySelector quitSelector $ Document.toParentNode document
        quitListener <- ET.eventListener (\_ -> Channel.send quitChannel true)
        let
          socketSignal = Channel.subscribe loadedContext.socketChannel

          gameTickSignal = GameTick <$> Channel.subscribe ticksChannel

          quitSignal = Channel.subscribe quitChannel

          gameStartedSignal = Channel.subscribe gameStartedChannel

          gameStateSignal =
            foldp
              ( \msg lc -> case msg of
                  Input i -> handleClientCommand lc i
                  GameTick tick -> handleTick (lc { hasError = tick.hasError }) tick.time
                  Ws str -> either (handleServerError lc) (handleServerMessage lc) $ readJSON str
              )
              loadedContext
              $ gameTickSignal
              <> (Input <$> gameInput)
              <> (Ws <$> socketSignal)

          gameSignal = (\lc -> lc.game) <$> gameStateSignal
        -- Feed the current time into the game state in a regulated manner
        -- Could probably do this whole thing with Signal.now and Signal.every
        -- if Signal.now wasn't arbitrary
        -- Once I refactor that massive LocalContext object I probably can as I won't need to init up front
        runSignal
          $ ( \_ -> do
                Milliseconds now <- Instant.unInstant <$> Now.now
                socketState <- WS.readyState socket
                Channel.send ticksChannel { time: now, hasError: socketState == RS.Closing || socketState == RS.Closed }
            )
          <$> tickSignal
        -- Send player input up to the server
        runSignal $ (\cmd -> safeSend socket $ writeJSON $ ClientCommand cmd) <$> gameInput
        -- Handle quitting manually
        runSignal
          $ ( \quit ->
                if quit then do
                  _ <- safeSend socket $ writeJSON Quit
                  _ <- Location.setHref "/" location
                  pure unit
                else
                  pure unit
            )
          <$> quitSignal
        maybe (pure unit) (\element -> ET.addEventListener ETS.click quitListener true $ Element.toEventTarget element) quitElement
        runSignal $ (\lc -> safeSend socket $ writeJSON $ Ping lc.game.lastTick) <$> sampleOn pingSignal gameStateSignal
        runSignal
          $ ( \lc -> case lc.info of
                Just info -> Rendering.init info.playerId gameSignal
                Nothing -> pure unit -- todo: avoid this
            )
          <$> sampleOn gameStartedSignal gameStateSignal
    )

safeSend :: WS.WebSocket -> String -> Effect Unit
safeSend ws str = do
  state <- WS.readyState ws
  case state of
    RS.Open -> WS.sendString ws str
    _ -> pure unit

handleServerError :: forall a. LocalContext -> a -> LocalContext
handleServerError lc _ = lc { hasError = true }

handleServerMessage :: LocalContext -> ServerMsg -> LocalContext
handleServerMessage lc (Welcome info) = lc { info = Just { playerId: wrap info.playerId, gameUrl: info.gameUrl } }

handleServerMessage lc@{ info: Just info } msg = case msg of
  Sync gameSync ->
    if not lc.isStarted then
      let
        newGame = Main.fromSync lc.now gameSync
      in
        lc
          { game = newGame
          , serverTick = gameSync.tick
          , isStarted = true
          }
    else
      let
        game = lc.game --Trace.trace { msg: "pre", game: lc.game } \_ -> lc.game

        updated = Main.mergeSyncInfo game gameSync -- Trace.trace {msg: "sync", sync: gameSync } \_ -> Main.mergeSyncInfo game gameSync

        result = lc { game = updated, serverTick = gameSync.tick } --Trace.trace {msg: "after", game: updated } \_ -> lc { game = updated, serverTick = gameSync.tick }
      in
        result
  PlayerSync sync -> lc { game = Main.mergePlayerSync lc.game sync }
  ServerCommand { id, cmd: cmd } ->
    if id == info.playerId then
      lc
    else
      lc { game = fst $ Main.sendCommand id (expand cmd) lc.game }
  ServerEvents evs -> lc { game = foldl (\a i -> fst $ Main.handleEvent a i) lc.game evs }
  PlayerAdded id -> lc { game = Main.addPlayer id lc.game }
  PlayerRemoved id -> lc { game = Main.removePlayer id lc.game }
  Pong tick -> lc { tickLatency = lc.game.lastTick - tick }
  Welcome _ -> lc

handleServerMessage lc _ = lc

handleClientCommand :: LocalContext -> Variant EntityCommand -> LocalContext
handleClientCommand lc@{ info: Just { playerId }, game } msg = lc { game = fst $ Main.sendCommand playerId (expand msg) game }

handleClientCommand lc _ = lc

handleTick :: LocalContext -> Number -> LocalContext
handleTick context@{ game, socket } now =
  let
    newGame = fst $ Main.tick now game
  in
    context { game = newGame, now = now }

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
