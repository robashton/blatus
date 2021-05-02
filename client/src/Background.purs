module Blatus.Client.Background where

import Prelude
import Data.Int (ceil, floor, toNumber)
import Data.List (range)
import Data.Traversable (traverse)
import Effect (Effect)
import Graphics.Canvas (CanvasGradient, Context2D)
import Graphics.Canvas as Context2D
import Blatus.Client.Camera (CameraViewport)
import Sisy.Runtime.Scene (Game)

tileWidth :: Number
tileWidth = 100.0

tileHeight :: Number
tileHeight = 100.0

foreign import setGradientStrokeStyle :: Context2D -> CanvasGradient -> Effect Unit

-- we'll need to actually an oversized offscreen context for this
-- and render only when leftTile,topTile,rightTile,bottomTile changes
render :: forall cmd ev entity. CameraViewport -> Game cmd ev entity -> Context2D -> Effect Unit
render viewport@{ left, right, top, bottom } game@{ world: { x, y, width, height } } ctx = do
  gradient <- Context2D.createRadialGradient ctx { x0: 0.0, y0: 0.0, r0: 200.0, x1: 0.0, y1: 0.0, r1: 2000.0 }
  _ <- Context2D.addColorStop gradient 0.0 "#fff"
  _ <- Context2D.addColorStop gradient 0.5 "#00f"
  _ <- Context2D.addColorStop gradient 1.0 "#f00"
  _ <- setGradientStrokeStyle ctx gradient
  _ <- Context2D.setLineWidth ctx 2.0
  _ <- Context2D.beginPath ctx
  _ <-
    traverse
      ( \tileX -> do
          let
            lineX = tileToHorizontal tileX
          _ <-
            traverse
              ( \tileY -> do
                  let
                    topY = (tileToVertical tileY) - 10.0

                    bottomY = topY + 20.0
                  _ <- Context2D.moveTo ctx lineX topY
                  _ <- Context2D.lineTo ctx lineX bottomY
                  pure unit
              )
              vertical
          pure unit
      )
      horizontal
  _ <-
    traverse
      ( \tileY -> do
          let
            lineY = tileToVertical tileY
          _ <-
            traverse
              ( \tileX -> do
                  let
                    leftX = (tileToHorizontal tileX) - 10.0

                    rightX = leftX + 20.0
                  _ <- Context2D.moveTo ctx leftX lineY
                  _ <- Context2D.lineTo ctx rightX lineY
                  pure unit
              )
              horizontal
          pure unit
      )
      vertical
  _ <- Context2D.stroke ctx
  pure unit
  where
  tileToHorizontal tile = (toNumber tile) * tileWidth + x

  tileToVertical tile = (toNumber tile) * tileHeight + y

  horizontal = range leftTile rightTile

  vertical = range topTile bottomTile

  leftTile = max 0 $ floor $ (left - x) / tileWidth

  topTile = max 0 $ floor $ (top - y) / tileHeight

  rightTile = ceil $ ((min right $ x + width) - x) / tileWidth

  bottomTile = ceil $ ((min bottom $ y + height) - y) / tileHeight
