module Pure.Main where

import Prelude

import Data.Either (isLeft, isRight)
import Data.Filterable (filterMap)
import Data.Foldable (foldl)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Monoid (guard)
import Data.Newtype (unwrap, wrap)
import Data.Traversable (for)
import Effect (Effect)
import Effect.Timer (setTimeout)
import Graphics.Canvas (clearRect, fillRect)
import Graphics.Canvas as Canvas
import Math (pi) as Math
import Pure.Camera (Camera, CameraViewport, applyViewport, setupCamera, viewportFromConfig)
import Pure.Game (EntityCommand(..), Game, initialModel, tick)
import Pure.Game (sendCommand) as Game
import Signal (Signal, foldp, map4, runSignal, sampleOn)
import Signal.DOM (keyPressed)
import Signal.Time (every, second)
import Web.HTML as HTML
import Web.HTML.Window (requestAnimationFrame) as Window


type LocalContext = { renderContext :: Canvas.Context2D
                    , canvasElement :: Canvas.CanvasElement
                    , camera :: Camera
                    , window :: HTML.Window
                    , game :: Game
                    }

type InputState =  { isLeft :: Boolean
                , isRight :: Boolean
                , isUp :: Boolean
                , isDown :: Boolean
  }

inputSignal :: Effect (Signal InputState)
inputSignal =
    map4 (\l u r d -> {  isLeft: l, isUp: u, isRight: r, isDown: d }) <$> (keyPressed 37) <*> (keyPressed 38) <*> (keyPressed 39) <*> (keyPressed 40)

tickSignal :: Effect (Signal InputState)
tickSignal = sampleOn (every $ second / 30.0) <$> inputSignal

main :: Effect Unit
main =  do
  maybeCanvas <- Canvas.getCanvasElementById "target"
  case  maybeCanvas of
       Just canvasElement -> do
          window <- HTML.window
          renderContext <- Canvas.getContext2D canvasElement
          canvasWidth <- Canvas.getCanvasWidth canvasElement
          canvasHeight <- Canvas.getCanvasHeight canvasElement
          input <- tickSignal
          let camera = setupCamera { width: canvasWidth, height: canvasHeight }
              game = initialModel
              context = foldp tickContext { renderContext, canvasElement, camera, window, game } input
          runSignal (map scheduleRender context)
       Nothing ->
         pure unit


tickContext :: InputState -> LocalContext  -> LocalContext
tickContext input context@{ game, camera: { config } } = 
  updatedContext { game = tick $ foldl handleCommand game $ gatherCommandsFromInput input }
      where viewport = viewportFromConfig config 
            updatedContext = context { camera = { config, viewport } }

data ExternalCommand = 
  PlayerCommand EntityCommand

handleCommand :: Game -> ExternalCommand -> Game
handleCommand game external = 
  case external of
       PlayerCommand command -> Game.sendCommand (wrap "player") command game

-- Why didn't 'guard' work? :S
gatherCommandsFromInput :: InputState -> (List ExternalCommand)
gatherCommandsFromInput { isLeft, isUp, isRight, isDown } = 
  filterMap identity $ (if isLeft then Just $ PlayerCommand TurnLeft else Nothing) :
                       (if isRight then Just $ PlayerCommand TurnRight else Nothing)  :
                       (if isDown then Just $ PlayerCommand PushBackward else Nothing)  : 
                       (if isUp then Just $ PlayerCommand PushForward else Nothing)  : Nil
--  filterMap identity $ (guard isLeft (Just $ PlayerCommand TurnLeft)) :
--                       (guard isRight $ Just $ PlayerCommand TurnRight) : Nil
--

scheduleRender :: LocalContext -> Effect Unit
scheduleRender context = do
  _ <- Window.requestAnimationFrame (render context) context.window
  pure unit

prepareScene :: CameraViewport -> Game -> Game
prepareScene viewport game = game -- TODO: Update renderables/occlude/etc

render :: LocalContext -> Effect Unit
render { camera: { viewport, config: { target: { width, height }} }, game, renderContext } = do
  _ <- Canvas.clearRect renderContext { x: 0.0, y: 0.0, width, height }
  _ <- Canvas.save renderContext
  _ <- applyViewport viewport renderContext
  _ <- renderScene game renderContext
  _ <- Canvas.restore renderContext
  pure unit

renderScene :: Game -> Canvas.Context2D -> Effect Unit
renderScene { entities } ctx = do
  _ <- for entities \{ location, renderables, rotation } -> Canvas.withContext ctx $ do
         _ <- Canvas.translate ctx { translateX: location.x, translateY: location.y }
         _ <- Canvas.rotate ctx (rotation * 2.0 * Math.pi)
         _ <- for renderables \{ transform, color, rotation: rr } -> Canvas.withContext ctx $ do
                _ <- Canvas.translate ctx { translateX: transform.x, translateY:  transform.y }
                _ <- Canvas.rotate ctx (rr * 2.0 * Math.pi)
                _ <- Canvas.translate ctx { translateX: (-transform.x), translateY: (-transform.y) }
                _ <- Canvas.setFillStyle ctx (unwrap color) 
                _ <- Canvas.fillRect ctx transform
                pure unit
         Canvas.translate ctx { translateX: (-location.x), translateY: (-location.y) }
  pure unit
