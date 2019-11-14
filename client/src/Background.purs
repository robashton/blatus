module Pure.Background where


import Prelude
import Pure.Camera (CameraViewport)

import Data.Int (ceil, floor, toNumber)
import Data.List (range)
import Data.Traversable (traverse)
import Effect (Effect)
import Graphics.Canvas (CanvasGradient, Context2D)
import Graphics.Canvas as Context2D
import Pure.Game (Game)

tileWidth :: Number
tileWidth = 100.0

tileHeight :: Number
tileHeight = 100.0

foreign import setGradientStrokeStyle :: Context2D -> CanvasGradient -> Effect Unit

render :: CameraViewport -> Game -> Context2D -> Effect Unit
render viewport@{  left, right, top, bottom } game@{ world: { x, y , width, height } } ctx = do
  gradient <- Context2D.createRadialGradient ctx { x0: 0.0, y0: 0.0, r0: 200.0, x1: 0.0, y1: 0.0, r1: 2000.0 }
  _ <- Context2D.addColorStop gradient 0.0 "#fff"
  _ <- Context2D.addColorStop gradient 0.5 "#00f"
  _ <- Context2D.addColorStop gradient 1.0 "#f00"
  _ <- setGradientStrokeStyle ctx gradient
  _ <- Context2D.setLineWidth ctx 5.0
  _ <- Context2D.beginPath ctx
  _ <- traverse (\tileX -> do
                    let lineX = (toNumber tileX) * tileWidth + x
                    _ <- Context2D.moveTo ctx lineX top
                    _ <- Context2D.lineTo ctx lineX bottom
                    pure unit
                    ) horizontal
  _ <- traverse (\tileY -> do
                    let lineY = (toNumber tileY) * tileHeight + y
                    _ <- Context2D.moveTo ctx left lineY
                    _ <- Context2D.lineTo ctx right $ lineY
                    pure unit) vertical
  _ <- Context2D.stroke ctx
  pure unit
  where
    horizontal = range leftTile rightTile
    vertical = range topTile bottomTile
    leftTile = floor $ (left - x) / tileWidth
    topTile = floor $ (top - y) / tileHeight
    rightTile = ceil $ (right - x) / tileWidth
    bottomTile = ceil $ (bottom - y) / tileHeight

