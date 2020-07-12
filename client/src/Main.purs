module Pure.Main where

import Prelude

import Assets (AssetPackage)
import Assets (AssetPackage, load) as Assets
import Data.Either (hush, isLeft, isRight)
import Data.Filterable (filterMap)
import Data.Foldable (foldl)
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
import Pure.Game (Game, entityById, foldEvents, initialModel, tick)
import Pure.Entity (EntityCommand(..))
import Pure.Game (sendCommand) as Game
import Signal (Signal, foldp, map4, map5, runSignal, sampleOn, squigglyMap)
import Signal as Signal
import Signal.DOM (keyPressed)
import Signal.Time (every, second)
import Signal.Channel as Channel
import Web.HTML as HTML
import Web.HTML.Window (requestAnimationFrame) as Window

import Web.Event.EventTarget as EET
import Web.Socket.Event.EventTypes as WSET
import Web.Socket.Event.MessageEvent as ME
import Web.Socket.WebSocket as WS


type LocalContext = { renderContext :: Canvas.Context2D
                    , canvasElement :: Canvas.CanvasElement
                    , offscreenContext :: Canvas.Context2D
                    , offscreenCanvas :: Canvas.CanvasElement
                    , assets :: Assets.AssetPackage
                    , camera :: Camera
                    , window :: HTML.Window
                    , game :: Game
                    }

type InputState =  { isLeft :: Boolean
                , isRight :: Boolean
                , isUp :: Boolean
                , isDown :: Boolean
                , isFiring :: Boolean
                }

inputSignal :: Effect (Signal InputState)
inputSignal =
    map5 (\l u r d f -> {  isLeft: l, isUp: u, isRight: r, isDown: d, isFiring: f }) <$> (keyPressed 37) <*> (keyPressed 38) <*> (keyPressed 39) <*> (keyPressed 40) <*> (keyPressed 32)


data LoopMsg = Input InputState
             | Ws String


tickSignal :: Effect (Signal LoopMsg)
tickSignal = squigglyMap Input <$> sampleOn (every $ second / 30.0) <$> inputSignal

--wsSignal :: Effect (Signal String)


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
          canvasHeight <- Canvas.getCanvasHeight canvasElement
          let camera = setupCamera { width: canvasWidth, height: canvasHeight }
              game = initialModel
          cb $ { offscreenContext, offscreenCanvas, renderContext, assets, canvasElement, camera, window, game}
  
-- But we're going to use signalling for game input/etc as that's quite pretty
-- even if we haven't worked out how to adjust for elapsed time during render/frame-rate yet in that world
-- I think what we need to do here
-- create a global message type

-- Tick -> Do a logic loop based on the current input (pull that the input signal as we need it)
        -- Fire another Tick in  (second / 30.0) - elapsedTimeSinceLastTickEnd

-- WebSocketMessage -> Handle it by firing it through the game

-- RenderRequest -> Just render the curreent scene... (requestAnimationFrame)

-- Probably need to use channels for the render message rather than a continual stream of scenes
main :: Effect Unit
main =  do
  load (\loadedContext -> do
          socketChannel <- Channel.channel $ Ws ""
          renderChannel <- Channel.channel 0
          socket <- createSocket "ws://localhost:3000/game/messaging" $ Ws >>> Channel.send socketChannel
          _ <- Window.requestAnimationFrame (Channel.send renderChannel 0) loadedContext.window
          gameTick <- tickSignal
          let socketSignal = Channel.subscribe socketChannel
              mergedSignals = Signal.merge gameTick socketSignal

              gameStateSignal = foldp (\msg lc -> 
                                case msg of
                                  Input i -> tickContext i lc
                                  Ws msg -> 
                                    -- parse message
                                    -- pass into game world 
                                    -- 
                                    lc
                                  ) loadedContext mergedSignals
      
          -- We'll need to run that gameStateSignal
          -- and pump any side effects up the web socket
          -- or update the UI and all that jazz

          -- The render loop, meanwhile, is separate
          -- And just pulls whatever is current off the signal and displays it
          runSignal (map (\_ -> do
                    latestState <- Signal.get gameStateSignal
                    render latestState
                    _ <- Window.requestAnimationFrame (Channel.send renderChannel 0) loadedContext.window
                    pure unit
                    ) $ Channel.subscribe renderChannel)
      )

tickContext :: InputState -> LocalContext  -> LocalContext
tickContext input context@{ game, camera: { config } } = 
  updatedContext { game = foldEvents $ tick $ foldl handleCommand game $ gatherCommandsFromInput input }
      where viewport = viewportFromConfig $ trackPlayer game config 
            updatedContext = context { camera = { config, viewport } }

trackPlayer :: Game -> CameraConfiguration -> CameraConfiguration
trackPlayer game config = 
  maybe config (\player -> config { lookAt = player.location,
                                    distance = 500.0 + (abs player.velocity.x + abs player.velocity.y) * 10.0
                                    }) $ entityById (wrap "player") game

data ExternalCommand = 
  PlayerCommand EntityCommand

handleCommand :: Game -> ExternalCommand -> Game
handleCommand game external = 
  case external of
       PlayerCommand command -> foldEvents $ Game.sendCommand (wrap "player") command game

-- Why didn't 'guard' work? :S
gatherCommandsFromInput :: InputState -> (List ExternalCommand)
gatherCommandsFromInput { isLeft, isUp, isRight, isDown, isFiring } = 
  filterMap identity $ (if isLeft then Just $ PlayerCommand TurnLeft else Nothing) :
                       (if isRight then Just $ PlayerCommand TurnRight else Nothing)  :
                       (if isDown then Just $ PlayerCommand PushBackward else Nothing)  : 
                       (if isUp then Just $ PlayerCommand PushForward else Nothing) : 
                       (if isFiring then Just $ PlayerCommand FireBullet else Nothing)  : Nil

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
