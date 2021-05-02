module Blatus.Server.Timing where

import Effect (Effect)

foreign import currentMs :: Effect Int
