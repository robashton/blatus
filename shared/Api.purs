module Blatus.Api where

type RunningGame
  = { id :: String
    , name :: String
    , startedBy :: String
    , public :: Boolean
    }
