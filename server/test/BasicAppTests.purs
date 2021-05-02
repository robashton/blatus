module Test.BasicAppTests where

import Prelude
import Control.Monad.Free (Free)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..), isJust)
import Erl.Data.Tuple (snd)
import Erl.Test.EUnit (TestF, setupTeardown, suite, test)
import Test.Assert (assertTrue')
import Test.Support.App as App
import Test.Support.Requests as Requests

tests :: Free TestF Unit
tests = do
  setupTeardown App.startApp App.stopApp do
    suite "Basic app stuff" do
      test "Can at least request the bloody home page" do
        page <- snd <$> Requests.requestPage "/"
        assertTrue' "Was able to request the bloody home page" (isJust page)
