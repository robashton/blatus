module Sisy.Math where

import Prelude
import Math as Math

type Point
  = { x :: Number
    , y :: Number
    }

type Rect
  = { x :: Number
    , y :: Number
    , width :: Number
    , height :: Number
    }

centreRect :: Point -> Rect -> Rect
centreRect p rect =
  rect
    { x = p.x - (rect.width / 2.0)
    , y = p.y - (rect.height / 2.0)
    }

origin :: Point
origin = point 0.0 0.0

point :: Number -> Number -> Point
point x y = { x, y }

scalePoint :: Number -> Point -> Point
scalePoint factor p@{ x, y } = p { x = x * factor, y = y * factor }

lerp :: Point -> Point -> Number -> Point
lerp p1 p2 l =
  { x: p1.x + l * (p2.x - p1.x)
  , y: p1.y + l * (p2.y - p1.y)
  }

distance :: Point -> Point -> Number
distance x y = magnitude $ (y - x)

vectorBetween :: Point -> Point -> Point
vectorBetween s d = normalise (d - s)

normalise :: Point -> Point
normalise point@{ x, y } = { x: x / den, y: y / den }
  where
  den = magnitude point

magnitude :: Point -> Number
magnitude { x, y } = Math.sqrt $ (x * x) + (y * y)

rotationToVector :: Number -> Point
rotationToVector r = { x: xvel, y: yvel }
  where
  angle = r * Math.pi * 2.0

  xvel = (Math.cos angle)

  yvel = (Math.sin angle)
