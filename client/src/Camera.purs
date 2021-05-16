module Blatus.Client.Camera where

import Prelude
import Data.Maybe (maybe')
import Data.Ord (abs)
import Effect (Effect)
import Graphics.Canvas as Canvas
import Math (tan) as Math
import Sisy.Math (Point, point, Rect)
import Sisy.Runtime.Entity (EntityId(..))
import Sisy.Runtime.Scene (Game, entityById)

type CameraViewport
  = { left :: Number
    , right :: Number
    , top :: Number
    , bottom :: Number
    , width :: Number
    , height :: Number
    , scale :: Point
    , aspectRatio :: Number
    }

type CameraTarget
  = { width :: Number
    , height :: Number
    }

type CameraConfiguration
  = { lookAt :: Point
    , distance :: Number
    , fieldOfView :: Number
    , target :: CameraTarget
    }

type Camera
  = { config :: CameraConfiguration
    , viewport :: CameraViewport
    }

type EntityState r
  = ( velocity :: Point | r )

canvasToWorld :: Camera -> Point -> Point
canvasToWorld c p =
  { x: (p.x / c.viewport.scale.x) + c.viewport.left
  , y: (p.y / c.viewport.scale.y) + c.viewport.top
  }

setupCamera :: CameraTarget -> Camera
setupCamera target =
  let
    config =
      { lookAt: point 0.0 0.0
      , distance: 1000.0
      , fieldOfView: 3.141 / 4.0
      , target
      }
  in
    { config
    , viewport: viewportFromConfig config
    }

move :: Number -> Number -> CameraConfiguration -> CameraConfiguration
move x y config@{ lookAt } = config { lookAt = lookAt + point x y }

moveTo :: Number -> Number -> CameraConfiguration -> CameraConfiguration
moveTo x y config = config { lookAt { x = x, y = y } }

applyViewport :: CameraViewport -> Canvas.Context2D -> Effect Unit
applyViewport viewport ctx = do
  _ <- Canvas.scale ctx { scaleX: viewport.scale.x, scaleY: viewport.scale.y }
  _ <- Canvas.translate ctx { translateX: (-viewport.left), translateY: (-viewport.top) }
  pure unit

testRect :: CameraViewport -> Rect -> Boolean
testRect viewport { x, y, width, height } =
  if viewport.right < x then
    false
  else if viewport.bottom < y then
    false
  else if viewport.left > x + width then
    false
  else if viewport.top > y + height then
    false
  else
    true

viewportFromConfig :: CameraConfiguration -> CameraViewport
viewportFromConfig config =
  { aspectRatio
  , width
  , height
  , left
  , top
  , right
  , bottom
  , scale: point (config.target.width / width) (config.target.height / height)
  }
  where
  width = config.distance * Math.tan config.fieldOfView

  height = width / aspectRatio

  aspectRatio = config.target.width / config.target.height

  left = config.lookAt.x - (width / 2.0)

  top = config.lookAt.y - (height / 2.0)

  right = left + width

  bottom = top + height

trackEntity :: forall cmd ev entity. EntityId -> Game cmd ev (EntityState entity) -> Camera -> Camera
trackEntity playerId game camera@{ config } = camera { config = newConfig, viewport = newViewport }
  where
  newViewport = viewportFromConfig newConfig

  newConfig =
    maybe' (\_ -> config { distance = config.distance + 2.0 })
      ( \player ->
          let
            targetDistance = 750.0 + (abs player.velocity.x + abs player.velocity.y) * 20.0
          in
            config
              { lookAt = player.location
              , distance = config.distance + 0.02 * (targetDistance - config.distance)
              }
      )
      $ entityById playerId game
