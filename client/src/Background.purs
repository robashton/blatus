module Blatus.Client.Background where

import Prelude
import Blatus.Client.Camera (CameraViewport, Camera)
import Data.Int (ceil, floor, toNumber)
import Data.List (List(..), concat, concatMap, range)
import Data.Traversable (sequence, traverse)
import Debug (spy)
import Effect (Effect)
import Effect.Random (random)
import Graphics.Canvas (CanvasGradient, Context2D)
import Graphics.Canvas as Context2D
import Sisy.Math (Point)
import Sisy.Runtime.Scene (Game)

tileWidth :: Number
tileWidth = 100.0

tileHeight :: Number
tileHeight = 100.0

type Tile
  = { x :: Number
    , y :: Number
    , offsetX :: Number
    , offsetY :: Number
    }

type State
  = { width :: Number
    , height :: Number
    , tiles :: List Tile
    , scale :: Number
    }

foreign import setGradientStrokeStyle :: Context2D -> CanvasGradient -> Effect Unit

init :: forall cmd ev entity. Number -> Game cmd ev entity -> Effect State
init scale game@{ world: { x, y, width, height } } = do
  tiles <-
    concat
      <$> traverse
          ( \ix ->
              traverse
                ( \iy -> do
                    ox <- random
                    oy <- random
                    pure
                      { x: ((toNumber ix) * tileWidth) + x
                      , y: ((toNumber iy) * tileHeight) + y
                      , offsetX: ox * tileWidth
                      , offsetY: oy * tileWidth
                      }
                )
                vertical
          )
          horizontal
  pure
    { scale
    , tiles
    , width
    , height
    }
  where
  tileCountX = floor $ width / tileWidth

  tileCountY = floor $ height / tileHeight

  horizontal = range 0 tileCountX

  vertical = range 0 tileCountY

-- we'll need to actually an oversized offscreen context for this
-- and render only when leftTile,topTile,rightTile,bottomTile changes
render :: Camera -> State -> Context2D -> Effect Unit
render { config: { lookAt: { x: lax, y: lay } }, viewport: { left, right, top, bottom } } { scale, tiles, width, height } ctx = do
  let
    scaleX = lax * scale

    scaleY = lay * scale
  gradient <- Context2D.createRadialGradient ctx { x0: 0.0, y0: 0.0, r0: 200.0, x1: 0.0, y1: 0.0, r1: width + height }
  _ <- Context2D.addColorStop gradient 0.0 "#fff"
  _ <- Context2D.addColorStop gradient 0.5 "#00f"
  _ <- Context2D.addColorStop gradient 1.0 "#f00"
  _ <- setGradientStrokeStyle ctx gradient
  _ <- Context2D.setLineWidth ctx 2.0
  _ <- Context2D.beginPath ctx
  _ <-
    traverse
      ( \{ x: tileX, y: tileY, offsetX, offsetY } -> do
          let
            sx = (tileX + scaleX + offsetX)

            sy = (tileY + scaleY + offsetY)
          if sx < left then
            pure unit
          else if sx > right then
            pure unit
          else if sy < top then
            pure unit
          else if sy > bottom then
            pure unit
          else do
            _ <- Context2D.moveTo ctx sx sy
            _ <- Context2D.lineTo ctx (sx + 1.0) (sy + 1.0)
            pure unit
      )
      tiles
  _ <- Context2D.stroke ctx
  pure unit
