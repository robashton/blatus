module Pure.Logging where

import Prelude
import Effect (Effect)
import Erl.Atom (Atom, atom)
import Erl.Data.List (List, nil, (:))
import Logger as Logger

data LogDomain
  = Web
  | RunningGame

logDomain :: LogDomain -> List Atom
logDomain domain =
  (atom "pure_unit")
    : ( atom
          $ case domain of
              Web -> "web"
              RunningGame -> "running_game"
      )
    : nil

info :: forall r. LogDomain -> String -> { | r } -> Effect Unit
info domain = Logger.info <<< Logger.traceMetadata (logDomain domain)
