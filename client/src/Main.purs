module Pure.Main where

import Prelude

import Assets (AssetPackage)
import Assets (AssetPackage, load) as Assets
import Control.Monad.Except (runExcept)
import Data.Array as Array
import Data.DateTime.Instant as Instant
import Data.Either (either, hush)
import Data.Foldable (foldl, for_)
import Data.Int as Int
import Data.Map (lookup) as Map
import Data.Maybe (fromMaybe, maybe)
import Data.Newtype (unwrap, wrap)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (for, traverse)
import Data.Tuple (Tuple(..), fst)
import Debug.Trace as Trace
import Effect (Effect)
import Effect.Aff (runAff_)
import Effect.Now as Now
import Foreign (readString)
import Graphics.Canvas as Canvas
import Math (abs)
import Math as Math
import Pure.Background (render) as Background
import Pure.BuiltIn.Bullets as Bullets
import Pure.BuiltIn.Explosions as Explosions
import Pure.Camera (Camera, CameraViewport, CameraConfiguration, applyViewport, setupCamera, viewportFromConfig)
import Pure.Comms (ServerMsg(..), ClientMsg(..))
import Pure.Comms as Comms
import Pure.Game.Main as Main
import Pure.Runtime.Scene (Game, entityById)
import Pure.Ticks as Ticks
import Pure.Types (EntityCommand(..), GameEvent)
import Signal (Signal, dropRepeats, foldp, runSignal, sampleOn)
import Signal as Signal
import Signal.Channel as Channel
import Signal.DOM (keyPressed, animationFrame)
import Signal.Time (every, second)
import Simple.JSON (readJSON, writeJSON)
import Web.DOM.Document as Document
import Web.DOM.Element as Element
import Web.DOM.Node as Node
import Web.DOM.NodeList as NodeList
import Web.DOM.ParentNode (QuerySelector(..), querySelector)
import Web.Event.EventTarget as EET
import Web.HTML as HTML
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window as Window
import Web.Socket.Event.EventTypes as WSET
import Web.Socket.Event.MessageEvent as ME
import Web.Socket.ReadyState as RS
import Web.Socket.WebSocket as WS


-- A pile of this is going to end up in Game.Main..
type LocalContext = { renderContext :: Canvas.Context2D
                    , canvasElement :: Canvas.CanvasElement
                    , offscreenContext :: Canvas.Context2D
                    , offscreenCanvas :: Canvas.CanvasElement
                    , assets :: Assets.AssetPackage
                    , camera :: Camera
                    , window :: HTML.Window
                    , game :: Main.State
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
              game = Main.init
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
      if not lc.isStarted then lc { game = Main.fromSync gameSync, clientTick = gameSync.tick, serverTick = gameSync.tick, isStarted = true }
      else 
        let 
            game = lc.game --Trace.trace { msg: "pre", game: lc.game } \_ -> lc.game
            updated = Main.mergeSyncInfo game gameSync -- Trace.trace {msg: "sync", sync: gameSync } \_ -> Main.mergeSyncInfo game gameSync
            result = lc { game = updated, serverTick = gameSync.tick } --Trace.trace {msg: "after", game: updated } \_ -> lc { game = updated, serverTick = gameSync.tick }
         in
           result

    PlayerSync sync ->
      lc { game = Main.mergePlayerSync lc.game sync }

    Welcome info ->
      lc { gameUrl = info.gameUrl, playerName = info.playerId }

    UpdatePlayerList list ->
      lc { playerList = list }

    ServerCommand { id, cmd } ->

      if (unwrap id) == lc.playerName then lc
        else lc  { game = fst $ Main.sendCommand id cmd lc.game }

    ServerEvents evs ->

      lc  { game = foldl Main.handleEvent lc.game evs }

    NewEntity sync ->
      lc { game  = Main.addEntity sync lc.game }

    EntityDeleted id ->
      lc { game  = Main.removeEntity id lc.game }

    Pong tick  ->
      lc { tickLatency = lc.clientTick - tick }

handleClientCommand :: LocalContext-> EntityCommand ->  LocalContext
handleClientCommand lc@{ playerName, game } msg =
  lc { game = fst $ Main.sendCommand (wrap playerName) msg game }

handleTick :: LocalContext -> Number  -> LocalContext
handleTick context@{ game, camera: { config }, playerName, socket, clientTick, ticks } now =
  let (Tuple framesToExecute newTicks) = Ticks.update now ticks
      viewport = viewportFromConfig $ trackPlayer playerName game.scene config 
      newGame = foldl (\acc x -> if x == 0 then acc else fst $ Main.tick acc) game $ Array.range 0 framesToExecute
      updatedContext = context { camera = { config, viewport } } in
    updatedContext { game = newGame, clientTick = clientTick + framesToExecute, ticks = newTicks }

trackPlayer :: String -> Game EntityCommand GameEvent -> CameraConfiguration -> CameraConfiguration
trackPlayer playerName game config = 
  maybe config (\player -> config { lookAt = player.location,
                                    distance = 500.0 + (abs player.velocity.x + abs player.velocity.y) * 10.0
                                    }) $ entityById (wrap playerName) game


render :: LocalContext -> Effect Unit
render context@{ camera: { viewport, config: { target: { width, height }} }, game, offscreenContext, offscreenCanvas, renderContext, assets } = do
  _ <- Canvas.clearRect offscreenContext { x: 0.0, y: 0.0, width, height }
  _ <- Canvas.save offscreenContext
  _ <- applyViewport viewport offscreenContext
  _ <- Background.render viewport game.scene offscreenContext 
  _ <- renderExplosions game.explosions offscreenContext
  _ <- renderBullets game.bullets offscreenContext
  _ <- renderScene game.scene assets offscreenContext
  _ <- Canvas.restore offscreenContext
  let image = Canvas.canvasElementToImageSource offscreenCanvas
  _ <- Canvas.clearRect renderContext { x: 0.0, y: 0.0, width, height }
  _ <- Canvas.drawImage renderContext image 0.0 0.0
  pure unit

prepareScene :: forall cmd ev. CameraViewport -> Game cmd ev -> Game cmd ev
prepareScene viewport game = game 

renderExplosions :: Explosions.State -> Canvas.Context2D -> Effect Unit
renderExplosions state ctx = do
  _ <- Canvas.setFillStyle ctx "#0ff"
  _ <- Canvas.beginPath ctx
  _ <- traverse (\b -> do
        let radius = (Int.toNumber b.age) + 2.0
        _ <- Canvas.moveTo ctx (b.location.x + radius) b.location.y
        _ <- Canvas.arc ctx { x: b.location.x
                            , y: b.location.y 
                            , start: 0.0 
                            , end : (2.0 * Math.pi)
                            , radius: radius
                            } 
        _ <- Canvas.fill ctx
        pure unit
     ) state.explosions
  Canvas.fill ctx

renderBullets :: forall ev. Bullets.State ev -> Canvas.Context2D -> Effect Unit
renderBullets state ctx = do
  _ <- Canvas.setFillStyle ctx "#0ff"
  _ <- Canvas.beginPath ctx
  _ <- traverse (\b -> do
        _ <- Canvas.moveTo ctx (b.location.x + 2.5) b.location.y
        _ <- Canvas.arc ctx { x: b.location.x
                            , y: b.location.y 
                            , start: 0.0 
                            , end : (2.0 * Math.pi)
                            , radius: 2.5
                            } 
        _ <- Canvas.fill ctx
        pure unit
     ) state.bullets
  Canvas.fill ctx
  


renderScene :: forall cmd ev. Game cmd ev -> AssetPackage -> Canvas.Context2D -> Effect Unit
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
