module Test.Support.App where

import Prelude
import Effect (Effect)
import Erl.Atom (Atom, atom)

startApp :: Effect Unit
startApp = do
  startImpl $ atom "gproc"
  setConfigImpl (atom "blatus") (atom "web_port") 3000
  startImpl $ atom "gun"
  startImpl $ atom "blatus"

stopApp :: Effect Unit
stopApp = do
  stopImpl $ atom "blatus"
  stopImpl $ atom "gun"
  stopImpl $ atom "gproc"

foreign import startImpl :: Atom -> Effect Unit

foreign import stopImpl :: Atom -> Effect Unit

foreign import setConfigImpl :: forall v. Atom -> Atom -> v -> Effect Unit
