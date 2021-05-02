module Test.Support.Requests where

import Data.Maybe (Maybe)
import Effect (Effect)
import Erl.Atom (Atom)
import Erl.Data.Tuple (Tuple2)

foreign import requestPage :: String -> Effect (Tuple2 Atom (Maybe String))
