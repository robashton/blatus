module Blatus.Client.Rendering where

import Prelude
import Blatus.Client.Assets (AssetPackage)
import Blatus.Client.Assets (AssetPackage, load) as Assets
import Blatus.Client.Background as Background
import Blatus.Client.Camera (Camera, CameraViewport, applyViewport, setupCamera)
import Blatus.Client.Camera as Camera
import Blatus.Main as Main
import Data.Either (hush)
import Data.Int as Int
import Data.Map (lookup) as Map
import Data.Maybe (fromMaybe)
import Data.Newtype (unwrap)
import Data.Traversable (for, traverse)
import Effect (Effect)
import Effect.Aff (runAff_)
import Graphics.Canvas as Canvas
import Math as Math
import Signal (Signal, runSignal, sampleOn)
import Signal as Signal
import Signal.DOM (animationFrame)
import Sisy.BuiltIn.Extensions.Bullets as Bullets
import Sisy.BuiltIn.Extensions.Explosions as Explosions
import Sisy.Math (Rect)
import Sisy.Runtime.Entity (EntityId)
import Sisy.Runtime.Scene (Game)

type State
  = { playerId :: EntityId
    , renderContext :: Canvas.Context2D
    , canvasElement :: Canvas.CanvasElement
    , offscreenContext :: Canvas.Context2D
    , offscreenCanvas :: Canvas.CanvasElement
    , assets :: Assets.AssetPackage
    , camera :: Camera
    , sf1 :: Background.State
    , sf2 :: Background.State
    , sf3 :: Background.State
    }

init :: EntityId -> Signal (Main.State) -> Effect Unit
init playerId gameSignal =
  runAff_
    ( \assets -> do
        maybeCanvas <- Canvas.getCanvasElementById "target"
        maybeOffscreen <- Canvas.getCanvasElementById "offscreen"
        fromMaybe (pure unit) $ renderSetup <$> maybeCanvas <*> maybeOffscreen <*> (hush assets)
    )
    $ Assets.load
  where
  renderSetup canvasElement offscreenCanvas assets = do
    renderContext <- Canvas.getContext2D canvasElement
    offscreenContext <- Canvas.getContext2D offscreenCanvas
    canvasWidth <- Canvas.getCanvasWidth canvasElement
    canvasHeight <- Canvas.getCanvasHeight canvasElement
    game <- Signal.get gameSignal
    sf1 <- Background.init 0.5 game.scene
    sf2 <- Background.init 0.3 game.scene
    sf3 <- Background.init 0.7 game.scene
    renderSignal <- animationFrame
    let
      camera = setupCamera { width: canvasWidth, height: canvasHeight }

      state =
        { renderContext
        , canvasElement
        , offscreenContext
        , offscreenCanvas
        , assets
        , camera
        , sf1
        , sf2
        , sf3
        , playerId
        }
    runSignal $ (render state) <$> sampleOn renderSignal gameSignal

render :: State -> Main.State -> Effect Unit
render context@{ camera: camera@{ viewport, config: { target: { width, height } } }, offscreenContext, offscreenCanvas, renderContext, assets, sf1, sf2, sf3 } game = do
  _ <- Canvas.clearRect offscreenContext { x: 0.0, y: 0.0, width, height }
  _ <- Canvas.save offscreenContext
  _ <- applyViewport viewport offscreenContext
  _ <- Background.render camera sf1 offscreenContext
  _ <- Background.render camera sf2 offscreenContext
  _ <- Background.render camera sf3 offscreenContext
  _ <- renderExplosions game.explosions offscreenContext
  _ <- renderBullets game.bullets offscreenContext
  _ <- renderScene viewport game.scene assets offscreenContext
  _ <- Canvas.restore offscreenContext
  let
    image = Canvas.canvasElementToImageSource offscreenCanvas
  _ <- Canvas.clearRect renderContext { x: 0.0, y: 0.0, width, height }
  _ <- Canvas.drawImage renderContext image 0.0 0.0
  pure unit

prepareScene :: forall cmd ev entity. CameraViewport -> Game cmd ev entity -> Game cmd ev entity
prepareScene viewport game = game

renderExplosions :: Explosions.State -> Canvas.Context2D -> Effect Unit
renderExplosions state ctx = do
  _ <- Canvas.setFillStyle ctx "#0ff"
  _ <- Canvas.beginPath ctx
  _ <-
    traverse
      ( \b -> do
          let
            radius = (Int.toNumber b.age) + 2.0
          _ <- Canvas.moveTo ctx (b.location.x + radius) b.location.y
          _ <-
            Canvas.arc ctx
              { x: b.location.x
              , y: b.location.y
              , start: 0.0
              , end: (2.0 * Math.pi)
              , radius: radius
              }
          _ <- Canvas.fill ctx
          pure unit
      )
      state.explosions
  Canvas.fill ctx

renderBullets :: Bullets.State -> Canvas.Context2D -> Effect Unit
renderBullets state ctx = do
  _ <- Canvas.setFillStyle ctx "#0ff"
  _ <- Canvas.beginPath ctx
  _ <-
    traverse
      ( \b -> do
          _ <- Canvas.moveTo ctx (b.location.x + 2.5) b.location.y
          _ <-
            Canvas.arc ctx
              { x: b.location.x
              , y: b.location.y
              , start: 0.0
              , end: (2.0 * Math.pi)
              , radius: 2.5
              }
          _ <- Canvas.fill ctx
          pure unit
      )
      state.bullets
  Canvas.fill ctx

renderScene :: forall cmd ev entity. CameraViewport -> Game cmd ev ( aabb :: Rect | entity ) -> AssetPackage -> Canvas.Context2D -> Effect Unit
renderScene viewport { entities } assets ctx = do
  _ <-
    for entities \{ aabb, location, renderables, rotation } -> do
      if (Camera.testRect viewport aabb) then
        Canvas.withContext ctx
          $ do
              _ <- Canvas.translate ctx { translateX: location.x, translateY: location.y }
              _ <- Canvas.rotate ctx (rotation * 2.0 * Math.pi)
              _ <-
                for renderables \{ transform, color, image, rotation: rr, visible } ->
                  Canvas.withContext ctx
                    $ do
                        if visible then do
                          _ <- Canvas.translate ctx { translateX: transform.x, translateY: transform.y }
                          _ <- Canvas.rotate ctx (rr * 2.0 * Math.pi)
                          _ <- Canvas.translate ctx { translateX: (-transform.x), translateY: (-transform.y) }
                          _ <- Canvas.setFillStyle ctx (unwrap color)
                          _ <-
                            fromMaybe (Canvas.fillRect ctx transform)
                              $ map (\img -> Canvas.drawImageScale ctx img transform.x transform.y transform.width transform.height)
                              $ (flip Map.lookup assets)
                              =<< image
                          pure unit
                        else
                          pure unit
              Canvas.translate ctx { translateX: (-location.x), translateY: (-location.y) }
      else
        pure unit
  pure unit

--    updatedConfig = trackPlayer playerName newGame.scene config
--
--    viewport = viewportFromConfig updatedConfig
--
--    updatedContext = context { camera = { config: updatedConfig, viewport } }
--trackPlayer :: String -> Game EntityCommand GameEvent GameEntity -> CameraConfiguration -> CameraConfiguration
--trackPlayer playerName game config =
--  maybe' (\_ -> config { distance = config.distance + 2.0 })
--    ( \player ->
--        let
--          targetDistance = 750.0 + (abs player.velocity.x + abs player.velocity.y) * 20.0
--        in
--          config
--            { lookAt = player.location
--            , distance = config.distance + 0.02 * (targetDistance - config.distance)
--            }
--    )
--    $ entityById (wrap playerName) game
--
