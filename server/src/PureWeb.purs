module PureWeb
  ( startLink
  , init
  , serverName
  , State
  )
  where

import Prelude

import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..), isJust, maybe)
import Effect (Effect)
import Erl.Atom (atom)
import Erl.Cowboy.Req (ReadBodyResult(..), Req, binding, readBody, setBody)
import Erl.Data.Binary (Binary)
import Erl.Data.Binary.IOData (IOData, fromBinary, toBinary)
import Erl.Data.List (List, nil, (:))
import Erl.Data.Tuple (Tuple2, tuple2)
import Pinto (ServerName(..), StartLinkResult)
import Pinto.Gen as Gen
import Simple.JSON (class WriteForeign, readJSON, writeJSON)
import Stetson (RestResult, StaticAssetLocation(..), StetsonHandler)
import Stetson as Stetson
import Stetson.Rest as Rest
import Unsafe.Coerce (unsafeCoerce)
import Shared.ServerRoutes as ServerRoutes

newtype State = State {}

type PureWebStartArgs = { webPort :: Int }

serverName :: ServerName State Unit
serverName = Local $ atom "pure_web"

startLink :: PureWebStartArgs -> Effect StartLinkResult
startLink args =
  Gen.startLink serverName $ init args

init :: PureWebStartArgs -> Gen.Init State Unit
init { webPort } = Gen.lift $ do
  _ <- Stetson.configure
    # Stetson.routes 
        ServerRoutes.apiRoute
        { "Assets": PrivDir "pure_ps" "www/assets"
        , "Art": PrivDir "pure_ps" "www/art"
        , "Index": PrivFile "pure_ps" "www/index.html"
        , "Game": PrivFile "pure_ps" "www/game.html"
        }
    # Stetson.port webPort
    # Stetson.bindTo 0 0 0 0
    # Stetson.startClear "http_listener"
  pure $ State {}
