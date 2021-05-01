module Test.Main where

import Prelude
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Erl.Data.List (List)
import Erl.Test.EUnit (TestSet)
import Erl.Tests.EUnit.Discovery (findTests, defaultOptions)

main_test_ :: List TestSet
main_test_ =
  let
    _ = unsafePerformEffect filterSasl
  in
    unsafePerformEffect $ findTests "server/test" defaultOptions

foreign import filterSasl :: Effect Unit
