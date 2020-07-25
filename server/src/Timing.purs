module Pure.Timing where

import Prelude
import Effect (Effect)

foreign import currentMs :: Effect Int
