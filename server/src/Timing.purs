module Pure.Timing where

import Effect (Effect)

foreign import currentMs :: Effect Int
