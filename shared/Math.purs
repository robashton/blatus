module Pure.Math where

import Prelude
import Math as Math

type Point = { x :: Number
            ,  y :: Number
}

type Rect = { x :: Number
            , y :: Number
            , width :: Number
            , height :: Number
            }

point  :: Number -> Number -> Point
point x y = { x, y }

scalePoint :: Number -> Point -> Point
scalePoint factor p@{ x, y } = p { x = x * factor, y = y * factor }

lerp :: Point -> Point -> Point
lerp p1 p2 = 
  { x : (p1.x + p2.x) / 2.0
  , y : (p1.y + p2.y) / 2.0
  }

rotationToVector :: Number -> Point
rotationToVector r = { x: xvel, y: yvel }
      where 
        angle = r  * Math.pi * 2.0
        xvel = (Math.cos angle) 
        yvel = (Math.sin angle)

