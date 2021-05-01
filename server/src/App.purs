module Pure.App where

import Effect.Uncurried (EffectFn2)
import Erl.Atom (Atom)
import Erl.Data.List (List)
import Foreign (Foreign)
import Pinto.App as App
import Pure.PrimarySup as Pure.PrimarySup

start :: forall args. EffectFn2 Atom (List args) Foreign
start = App.simpleStart Pure.PrimarySup.startLink
