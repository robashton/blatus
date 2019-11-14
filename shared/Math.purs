module Pure.Math where

import Prelude

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

