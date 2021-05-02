module Blatus.Server.App where

import Effect.Uncurried (EffectFn2)
import Erl.Atom (Atom)
import Erl.Data.List (List)
import Foreign (Foreign)
import Pinto.App as App
import Blatus.Server.PrimarySup as PrimarySup

start :: forall args. EffectFn2 Atom (List args) Foreign
start = App.simpleStart PrimarySup.startLink
