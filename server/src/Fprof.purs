module  Fprof where

import Prelude
import Effect (Effect)

foreign import start :: Effect Unit
foreign import stop :: Effect Unit
