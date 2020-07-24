module Pure.Main where

import Prelude

import Debug.Trace as Trace
import Assets (AssetPackage)
import Assets (AssetPackage, load) as Assets
import Data.Either (hush, isLeft, isRight, either, Either(..))
import Data.Filterable (filterMap)
import Data.Foldable (foldl, foldM)
import Data.List (List(..), (:))
import Data.Map (lookup) as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Monoid (guard)
import Data.Newtype (unwrap, wrap)
import Control.Monad.Except (runExcept)
import Data.Traversable (for)
import Effect.Class (liftEffect)
import Data.Tuple (fst)
import Effect (Effect)
import Effect.Aff (runAff, runAff_)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Foreign (readString)
import Effect.Timer (setTimeout)
import Graphics.Canvas (CanvasElement, clearRect, fillRect, getContext2D)
import Graphics.Canvas as Canvas
import Math (abs)
import Math (pi) as Math
import Data.Foldable (for_)
import Pure.Background (render) as Background
import Pure.Camera (Camera, CameraViewport, CameraConfiguration, applyViewport, setupCamera, viewportFromConfig)
import Pure.Game (Game, entityById, foldEvents, initialModel, tick, addEntity, discardEvents)
import Pure.Entity (EntityCommand(..))
import Pure.Game (sendCommand) as Game
import Signal (Signal, foldp, map4, map5, runSignal, sampleOn, squigglyMap, dropRepeats)
import Signal as Signal
import Signal.DOM (keyPressed, animationFrame)
import Signal.Time (every, second)
import Signal.Channel as Channel
import Web.HTML as HTML
import Web.HTML.Window (requestAnimationFrame) as Window
import Simple.JSON (readJSON, writeJSON)
import Pure.Comms (ServerMsg(..), ClientMsg(..))
import Pure.Comms as Comms

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
                 | GameTick
                 | Ws String

tickSignal :: Signal GameLoopMsg
tickSignal = sampleOn (every $ second / 30.0) $ Signal.constant GameTick

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
          let camera = setupCamera { width: canvasWidth, height: canvasHeight }
              game = initialModel
          cb $ { offscreenContext, offscreenCanvas, renderContext, assets, canvasElement, camera, window, game, playerName: "", socket, socketChannel}
  

main :: Effect Unit
main =  do
  load (\loadedContext@{ socket } -> do
          gameInput <- inputSignal
          renderSignal <- animationFrame

          -- Just alter context state as messages come in
          let socketSignal = Channel.subscribe loadedContext.socketChannel

          let gameStateSignal = foldp (\msg lc ->  do
                                         case msg of
                                           Input i -> handleClientCommand lc i
                                           GameTick -> handleTick lc
                                           Ws str -> either (\err -> lc) (handleServerMessage lc) $ readJSON str
                                      ) loadedContext $ tickSignal <> (Input <$> gameInput) <> (Ws <$> socketSignal)

          -- Send player input up to the server
          runSignal $ (\cmd -> safeSend socket $ writeJSON $ ClientCommand cmd) <$> gameInput

          -- Tick as well
          runSignal $ (\cmd -> safeSend socket $ writeJSON $ ClientTick) <$> tickSignal
      
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
    InitialState gameSync ->
      lc { playerName = gameSync.playerName, game = Comms.gameFromSync gameSync }

    ServerCommand { id, cmd } ->

      lc  { game = foldEvents $ Game.sendCommand id cmd lc.game }

    NewEntity sync ->
      lc { game  = addEntity (Comms.entityFromSync sync) lc.game }

    ServerTick  ->
      lc

handleClientCommand :: LocalContext-> EntityCommand ->  LocalContext
handleClientCommand lc@{ playerName, game } msg =
  lc { game = discardEvents $ Game.sendCommand (wrap playerName) msg game }

handleTick :: LocalContext  -> LocalContext
handleTick context@{ game, camera: { config }, playerName, socket } =  do
  updatedContext { game = nextGame }
      where nextGame = discardEvents $ tick game 
            viewport = viewportFromConfig $ trackPlayer playerName game config 
            updatedContext = context { camera = { config, viewport } }

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
