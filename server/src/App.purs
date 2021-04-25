module Pure.App where

import Prelude
import Erl.Data.List (List)
import Effect.Uncurried (EffectFn2)
import Erl.Atom (Atom)
import Foreign (Foreign)
import Pinto.App as App
import Pure.PrimarySup as Pure.PrimarySup

start = App.simpleStart Pure.PrimarySup.startLink
