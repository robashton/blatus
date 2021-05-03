module Blatus.Api where

-- To seed a world..
type RunningGame
  = { id :: String
    , name :: String
    , startedBy :: String
    , public :: Boolean
    , numPlayers :: Int
    , width :: Int
    , height :: Int
    }
