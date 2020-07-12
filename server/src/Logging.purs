module Pure.Logging where

import Prelude
import Effect (Effect)
import Erl.Data.List (List, nil, (:))
import Logger as Logger
import Erl.Atom (Atom ,atom)

data LogDomain = Web

logDomain :: LogDomain -> List Atom
logDomain domain = (atom "pure_unit") : (atom $ case domain of 
                                        Web -> "web"
                                      ) : nil

info :: forall r. LogDomain -> String  -> { | r } -> Effect Unit
info domain text r = Logger.debug { domain: logDomain domain, text, type: Logger.Trace } r
