module Pure.Config where

import Effect (Effect)

foreign import readString_ :: String -> Effect String

foreign import readInt_ :: String -> Effect Int

foreign import readDirect_ :: forall a. String -> Effect a

webPort :: Effect Int
webPort = readInt_ "web_port"
