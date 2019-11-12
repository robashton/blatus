module Pure.Main where

import Prelude

import Data.List (List)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Traversable (for)
import Effect (Effect)
import Effect.Timer (setTimeout)
import Graphics.Canvas (clearRect, fillRect)
import Graphics.Canvas as Canvas
import Math (pi) as Math
import Pure.Camera (Camera, CameraViewport, applyViewport, setupCamera, viewportFromConfig)
import Pure.Game (EntityCommand(..), Game, initialModel, tick)
import Web.HTML as HTML
import Web.HTML.Window (requestAnimationFrame) as Window


type LocalContext = { renderContext :: Canvas.Context2D
                    , canvasElement :: Canvas.CanvasElement
                    , camera :: Camera
                    , window :: HTML.Window
                    , game :: Game
                    }

main :: Effect Unit
main =  do
  maybeCanvas <- Canvas.getCanvasElementById "target"
  case  maybeCanvas of
       Just canvasElement -> do
          window <- HTML.window
          renderContext <- Canvas.getContext2D canvasElement
          canvasWidth <- Canvas.getCanvasWidth canvasElement
          canvasHeight <- Canvas.getCanvasHeight canvasElement
          let camera = setupCamera { width: canvasWidth, height: canvasHeight }
              game = initialModel
          gameLoop { renderContext, canvasElement, camera, window, game }
       Nothing ->
         pure unit


data ExternalCommand = 
  PlayerCommand EntityCommand

gatherCommandsFromInput :: Effect (List ExternalCommand)
gatherCommandsFromInput = do
  left <- keyPressed 37 
  right <- keyPressed 39
  up <- keyPressed 38
  down <- keyPressed 40
  filterMap id ( (guard left $ Just $ PlayerCommand TurnLeft) : Nil )



gameLoop :: LocalContext -> Effect Unit
gameLoop local@{ game, camera: { config } } = do
  let viewport = viewportFromConfig config 
      updatedContext = local { camera = { config, viewport } }
  _ <- Window.requestAnimationFrame (render updatedContext) updatedContext.window
  commands_ <- gatherCommandsFromInput
  _ <- setTimeout 33 $ gameLoop $ updatedContext { game = tick $ foldL handleCommand updatedContext.game commands } -- TODO: Take into account how long rendering has taken and do that dance..
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
