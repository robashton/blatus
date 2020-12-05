module Pure.Main where

import Prelude

import Debug.Trace as Trace
import Assets (AssetPackage)
import Assets (AssetPackage, load) as Assets
import Data.Either (either, hush)
import Data.Array as Array
import Data.Foldable (foldl, for_)
import Data.Map (lookup) as Map
import Data.Maybe (fromMaybe, maybe)
import Data.Newtype (unwrap, wrap)
import Control.Monad.Except (runExcept)
import Data.Traversable (for, traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (runAff_)
import Foreign (readString)
import Graphics.Canvas as Canvas
import Math (abs)
import Math (pi) as Math
import Pure.Background (render) as Background
import Pure.Camera (Camera, CameraViewport, CameraConfiguration, applyViewport, setupCamera, viewportFromConfig)
import Pure.Game (Game, entityById, foldEvents, initialModel, tick, addEntity, removeEntity, discardEvents)
import Pure.Entity (EntityCommand(..))
import Pure.Game (sendCommand) as Game
import Signal (Signal, dropRepeats, foldp, runSignal, sampleOn)
import Signal as Signal
import Signal.DOM (keyPressed, animationFrame)
import Signal.Time (every, second)
import Signal.Channel as Channel
import Web.HTML as HTML
import Web.HTML.Window  as Window
import Web.HTML.HTMLDocument  as HTMLDocument
import Web.DOM.Document as Document
import Web.DOM.Element as Element
import Web.DOM.Node as Node
import Web.DOM.NodeList as NodeList
import Web.DOM.ParentNode (QuerySelector(..), querySelector)
import Simple.JSON (readJSON, writeJSON)
import Pure.Comms (ServerMsg(..), ClientMsg(..))
import Pure.Comms as Comms
import Pure.Ticks as Ticks
import Effect.Now as Now
import Data.DateTime.Instant as Instant
import Data.Time.Duration (Milliseconds(..))

import Web.Event.EventTarget as EET
import Web.Socket.Event.EventTypes as WSET
import Web.Socket.Event.MessageEvent as ME
import Web.Socket.WebSocket as WS
import Web.Socket.ReadyState as RS


type LocalContext = { renderContext :: Canvas.Context2D
                    , canvasElement :: Canvas.CanvasElement
                    , offscreenContext :: Canvas.Context2D
                    , offscreenCanvas :: Canvas.CanvasElement
                    , assets :: Assets.AssetPackage
                    , camera :: Camera
                    , window :: HTML.Window
                    , game :: Game
                    , playerName :: String
                    , socketChannel :: Channel.Channel String
                    , socket :: WS.WebSocket
                    , clientTick :: Int
                    , serverTick :: Int
                    , tickLatency :: Int
                    , ticks :: Ticks.State
                    , gameUrl :: String
                    , playerList :: Array Comms.PlayerListItem
                    , isStarted :: Boolean
                    }

rotateLeftSignal :: Effect (Signal EntityCommand)
rotateLeftSignal = do
  key <- keyPressed 37
  pure $ dropRepeats $ (\x -> if x then TurnLeft else StopTurnLeft) <$> key

thrustSignal :: Effect (Signal EntityCommand)
thrustSignal = do
  key <- keyPressed 38
  pure $ dropRepeats $ (\x -> if x then PushForward else StopPushForward) <$> key

rotateRightSignal :: Effect (Signal EntityCommand)
rotateRightSignal = do
  key <- keyPressed 39
  pure $ dropRepeats $ (\x -> if x then TurnRight else StopTurnRight) <$> key

brakeSignal :: Effect (Signal EntityCommand)
brakeSignal = do
  key <- keyPressed 40
  pure $ dropRepeats $ (\x -> if x then PushBackward else StopPushBackward) <$> key

fireSignal :: Effect (Signal EntityCommand)
fireSignal = do
  key <- keyPressed 32
  pure $ dropRepeats $ (\x -> if x then FireBullet else StopFiring) <$> key

inputSignal :: Effect (Signal EntityCommand)
inputSignal = do
  fs <- fireSignal
  rl <- rotateLeftSignal
  ts <- thrustSignal
  rr <- rotateRightSignal
  bs <- brakeSignal
  pure $ fs <> rl <> ts <> rr <> bs


data GameLoopMsg = Input EntityCommand
                 | GameTick Number
                 | Ws String

timePerFrame :: Number
timePerFrame = 1000.0 / 30.0

tickSignal :: Signal Unit
tickSignal = sampleOn (every $ second / 30.0) $ Signal.constant unit
  
pingSignal :: Signal Unit
pingSignal = sampleOn (every $ second) $ Signal.constant unit

uiUpdateSignal :: Signal Unit
uiUpdateSignal = sampleOn (every $ second * 2.0) $ Signal.constant unit

-- We're going to use Aff to make loading pretty instead of trying
-- to chain Signals around the place
-- TODO: Loading screen, convert maybes to eithers instead of vice versa, display errors
load ::  (LocalContext -> Effect Unit) -> Effect Unit
load cb = do
  runAff_ (\assets -> do
              maybeCanvas <- Canvas.getCanvasElementById "target"
              maybeOffscreen <- Canvas.getCanvasElementById "offscreen"
              fromMaybe (pure unit) $ prepareContexts <$> maybeCanvas <*> maybeOffscreen <*> (hush assets)
              ) $ Assets.load
  where prepareContexts canvasElement offscreenCanvas assets = do
          window <- HTML.window
          renderContext <- Canvas.getContext2D canvasElement
          offscreenContext <- Canvas.getContext2D offscreenCanvas
          canvasWidth <- Canvas.getCanvasWidth canvasElement
          socketChannel <- Channel.channel $ ""
          socket <- createSocket "ws://localhost:3000/messaging" $ Channel.send socketChannel
          canvasHeight <- Canvas.getCanvasHeight canvasElement
          Milliseconds now <- Instant.unInstant <$> Now.now

          let camera = setupCamera { width: canvasWidth, height: canvasHeight }
              game = initialModel
          cb $ { offscreenContext
               , offscreenCanvas
               , renderContext
               , assets
               , canvasElement
               , camera
               , window
               , game
               , playerName: "",
               gameUrl: ""
               , socket
               , socketChannel
               , clientTick: 0
               , serverTick: 0
               , tickLatency: 0
               , playerList: []
               , isStarted: false
               , ticks: Ticks.init now timePerFrame}
  
gameInfoSelector :: QuerySelector
gameInfoSelector = QuerySelector ("#game-info")

playerListSelector :: QuerySelector
playerListSelector = QuerySelector ("#player-list")

latencyInfoSelector :: QuerySelector
latencyInfoSelector = QuerySelector ("#latency-info")

main :: Effect Unit
main =  do
  load (\loadedContext@{ socket, window } -> do
          gameInput <- inputSignal
          renderSignal <- animationFrame
          document <- HTMLDocument.toDocument <$> Window.document window
          Milliseconds start <- Instant.unInstant <$> Now.now
          ticksChannel <- Channel.channel start

          gameInfoElement <- querySelector gameInfoSelector $ Document.toParentNode document
          latencyInfoElement <- querySelector latencyInfoSelector $ Document.toParentNode document
          playerListElement <- querySelector playerListSelector $ Document.toParentNode document

          -- Just alter context state as messages come in
          let socketSignal = Channel.subscribe loadedContext.socketChannel
              gameTickSignal = GameTick <$> Channel.subscribe ticksChannel

          let gameStateSignal = foldp (\msg lc -> 
                                         case msg of
                                           Input i -> handleClientCommand lc i
                                           GameTick now -> handleTick lc now
                                           Ws str -> either (\err -> lc) (handleServerMessage lc) $ readJSON str
                                      ) loadedContext $ gameTickSignal <> (Input <$> gameInput) <> (Ws <$> socketSignal)

          -- Feed the current time into the game state in a regulated manner
          -- Could probably do this whole thing with Signal.now and Signal.every
          -- if Signal.now wasn't arbitrary
          -- Once I refactor that massive LocalContext object I probably can as I won't need to init up front
          runSignal $ (\_ -> do
            Milliseconds now <- Instant.unInstant <$> Now.now
            Channel.send ticksChannel now
            ) <$> tickSignal

          -- Send player input up to the server
          runSignal $ (\cmd -> safeSend socket $ writeJSON $ ClientCommand cmd) <$> gameInput

          -- Tick as well
          runSignal $ (\_ -> do

            lc <- Signal.get gameStateSignal

            -- Update the display
            _ <- maybe (pure unit) (\element -> do
                      Element.setAttribute "href" lc.gameUrl element
                      Node.setTextContent lc.gameUrl $ Element.toNode element) gameInfoElement

            _ <- maybe (pure unit) (\element -> do
                      Node.setTextContent ("Ping: " <> (show (lc.tickLatency * 33)) <> "ms") $ Element.toNode element) latencyInfoElement

            _ <- maybe (pure unit) (\element -> do
                       let node = Element.toNode element
                       existingChildren <- NodeList.toArray =<< Node.childNodes node
                       _ <- traverse (\child -> Node.removeChild child node) $ existingChildren
                       _ <- traverse (\player -> do
                           li <- Element.toNode <$> Document.createElement "li" document
                           Node.setTextContent (player.playerId <> ": " <> (show player.score)) li
                           Node.appendChild li node) $ lc.playerList



                       pure unit) playerListElement
            pure unit) <$> uiUpdateSignal 

          -- Tick
          runSignal $ (\_ -> do
            lc <- Signal.get gameStateSignal
            safeSend socket $ writeJSON $ Ping lc.clientTick ) <$> pingSignal 
      
          -- Take whatever the latest state is and render it every time we get a render frame request
          runSignal $ (\_ -> do
                          latestState <- Signal.get gameStateSignal
                          render latestState
                          pure unit
                         ) <$> renderSignal
      )

safeSend :: WS.WebSocket -> String -> Effect Unit
safeSend ws str = do
  state <- WS.readyState ws
  case state of
    RS.Open -> 
      WS.sendString ws str 
    _ ->
      pure unit
     

handleServerMessage :: LocalContext -> ServerMsg -> LocalContext
handleServerMessage lc msg =
  case msg of
    Sync gameSync ->
      if not lc.isStarted then lc { game = Comms.gameFromSync gameSync, clientTick = gameSync.tick, serverTick = gameSync.tick, isStarted = true }
      else 
        let 
            game = Trace.trace { msg: "pre", game: lc.game } \_ -> lc.game
            updated = Trace.trace {msg: "sync", sync: gameSync } \_ -> Comms.mergeSyncInfo game gameSync
            result = Trace.trace {msg: "after", game: updated } \_ -> lc { game = updated, serverTick = gameSync.tick }
         in
           result

    PlayerSync sync ->
      lc { game = Comms.mergePlayerSync lc.game sync }

    Welcome info ->
      lc { gameUrl = info.gameUrl, playerName = info.playerId }

    UpdatePlayerList list ->
      lc { playerList = list }

    ServerCommand { id, cmd } ->

      if (unwrap id) == lc.playerName then lc
        else lc  { game = discardEvents $ Game.sendCommand id cmd lc.game }

    ServerEvents evs ->

      lc  { game = foldEvents lc.game evs }

    NewEntity sync ->
      lc { game  = addEntity (Comms.entityFromSync sync) lc.game }

    EntityDeleted id ->
      lc { game  = removeEntity id lc.game }

    Pong tick  ->
      lc { tickLatency = lc.clientTick - tick }

handleClientCommand :: LocalContext-> EntityCommand ->  LocalContext
handleClientCommand lc@{ playerName, game } msg =
  lc { game = discardEvents $ Game.sendCommand (wrap playerName) msg game }

handleTick :: LocalContext -> Number  -> LocalContext
handleTick context@{ game, camera: { config }, playerName, socket, clientTick, ticks } now =
  let (Tuple framesToExecute newTicks) = Ticks.update now ticks
      viewport = viewportFromConfig $ trackPlayer playerName game config 
      newGame = foldl (\acc x -> if x == 0 then acc else discardEvents $ tick acc) game $ Array.range 0 framesToExecute
      updatedContext = context { camera = { config, viewport } } in
    updatedContext { game = newGame, clientTick = clientTick + framesToExecute, ticks = newTicks }

trackPlayer :: String -> Game -> CameraConfiguration -> CameraConfiguration
trackPlayer playerName game config = 
  maybe config (\player -> config { lookAt = player.location,
                                    distance = 500.0 + (abs player.velocity.x + abs player.velocity.y) * 10.0
                                    }) $ entityById (wrap playerName) game


render :: LocalContext -> Effect Unit
render context@{ camera: { viewport, config: { target: { width, height }} }, game, offscreenContext, offscreenCanvas, renderContext, assets } = do
  _ <- Canvas.clearRect offscreenContext { x: 0.0, y: 0.0, width, height }
  _ <- Canvas.save offscreenContext
  _ <- applyViewport viewport offscreenContext
  _ <- Background.render viewport game offscreenContext 
  _ <- renderScene game assets offscreenContext
  _ <- Canvas.restore offscreenContext
  let image = Canvas.canvasElementToImageSource offscreenCanvas
  _ <- Canvas.clearRect renderContext { x: 0.0, y: 0.0, width, height }
  _ <- Canvas.drawImage renderContext image 0.0 0.0
  pure unit

prepareScene :: CameraViewport -> Game -> Game
prepareScene viewport game = game 

renderScene :: Game -> AssetPackage -> Canvas.Context2D -> Effect Unit
renderScene { entities } assets ctx = do
  _ <- for entities \{ location, renderables, rotation } -> Canvas.withContext ctx $ do
         _ <- Canvas.translate ctx { translateX: location.x, translateY: location.y }
         _ <- Canvas.rotate ctx (rotation * 2.0 * Math.pi)
         _ <- for renderables \{ transform, color, image, rotation: rr } -> Canvas.withContext ctx $ do
                _ <- Canvas.translate ctx { translateX: transform.x, translateY:  transform.y }
                _ <- Canvas.rotate ctx (rr * 2.0 * Math.pi)
                _ <- Canvas.translate ctx { translateX: (-transform.x), translateY: (-transform.y) }
                _ <- Canvas.setFillStyle ctx (unwrap color) 
                _ <- fromMaybe (Canvas.fillRect ctx transform) 
                        $ map (\img -> Canvas.drawImageScale ctx img transform.x transform.y transform.width transform.height) 
                        $ (flip Map.lookup assets) =<< image 
                pure unit
         Canvas.translate ctx { translateX: (-location.x), translateY: (-location.y) }
  pure unit

createSocket :: String -> (String -> Effect Unit) -> Effect WS.WebSocket
createSocket url cb = do
  socket <- WS.create url []
  listener <- EET.eventListener \ev ->
    for_ (ME.fromEvent ev) \msgEvent ->
      for_ (runExcept $ readString $ ME.data_ msgEvent) cb
  EET.addEventListener WSET.onMessage listener false (WS.toEventTarget socket)
  pure socket

destroySocket  :: WS.WebSocket -> Effect Unit
destroySocket socket =  do
  _ <- WS.close socket
  pure unit
