module PureWeb
  ( startLink
  , init
  , serverName
  , State
  )
  where

import Prelude

import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..), isJust, maybe, fromMaybe)
import Control.Apply (lift3)
import Effect (Effect)
import Data.Map (fromFoldable)
import Data.Map as Map
import Erl.Data.Map as ErlMap
import Data.Tuple (Tuple(..))
import Erl.Atom (atom)
import Erl.Cowboy.Req (ReadBodyResult(..), Req, binding, readBody, setBody, readUrlEncodedBody, ReadUrlEncodedBodyResult(..), replyWithoutBody, StatusCode(..), setCookie)
import Erl.Data.Binary (Binary)
import Erl.Data.Binary.IOData (IOData, fromBinary, toBinary)
import Erl.Data.List (List, nil, (:))
import Erl.Data.Tuple (Tuple2, tuple2, uncurry2)
import Pinto (ServerName(..), StartLinkResult)
import Pinto.Gen as Gen
import Simple.JSON (class WriteForeign, readJSON, writeJSON)
import Stetson (RestResult, StaticAssetLocation(..), StetsonHandler, SimpleStetsonHandler)
import Stetson as Stetson
import Stetson.Rest as Rest
import Stetson.Routing (class GDispatch, gDispatch)
import Unsafe.Coerce (unsafeCoerce)
import Shared.ServerRoutes as ServerRoutes
import Pure.Api (RunningGame(..))
import Pure.Logging as Log
import Pure.RunningGameList as PureRunningGameList

newtype State = State {}

type PureWebStartArgs = { webPort :: Int }

serverName :: ServerName State Unit
serverName = Local $ atom "pure_web"

startLink :: PureWebStartArgs -> Effect StartLinkResult
startLink args =
  Gen.startLink serverName $ init args

init :: PureWebStartArgs -> Gen.Init State Unit
init { webPort } = Gen.lift $ do
  Log.info Log.Web "Starting up web server" { webPort }
  _ <- Stetson.configure
    # Stetson.routes 
        ServerRoutes.topRoute
        { "Assets": PrivDir "pure_unit" "www/assets"
        , "Art": PrivDir "pure_unit" "www/art"
        , "GameStart": gamesHandler
        , "GameJoin": gameJoinHandler
        , "GameJoinHtml": (\(id :: String) -> PrivFile "pure_unit" "www/join.html")
        , "GamePlay" : PrivFile "pure_unit" "www/game.html"
        , "Index" : PrivFile "pure_unit" "www/index.html"
        } 
    # Stetson.port webPort
    # Stetson.bindTo 0 0 0 0
    # Stetson.startClear "http_listener"
  pure $ State {}


gamesHandler :: SimpleStetsonHandler (List RunningGame)
gamesHandler =
  Rest.handler (\req -> do
    all <- PureRunningGameList.findAll
    Rest.initResult req all)
    # Rest.allowedMethods (\req state -> Rest.result (Stetson.POST :  Stetson.HEAD : Stetson.GET : Stetson.OPTIONS : nil) req state)
    # Rest.contentTypesProvided (\req state -> Rest.result (jsonWriter : nil) req state)
    # Rest.contentTypesAccepted (\req state -> Rest.result ((tuple2 "application/x-www-form-urlencoded" acceptForm) : nil) req state)
    where
          acceptForm = (\req state -> do
                       UrlEncodedBody kvs req2  <- readUrlEncodedBody req
                       let processed = fromFoldable $ map (uncurry2 Tuple) kvs
                       fromMaybe (Rest.result false (setBody "unable to comply" req) state) $
                         lift3 (\playerName gameName public -> do
                               Log.info Log.Web "Attempt to create game" { playerName, gameName, public }
                               gameId <- PureRunningGameList.create playerName gameName (public == "on")
                               ir <- replyWithoutBody (StatusCode 302) (ErlMap.fromFoldable [ (Tuple "Location" $ ServerRoutes.routeUrl ServerRoutes.GamePlay) ]) 
                                        $ setCookie "game_id" gameId $ setCookie "player-name" playerName req
                               Rest.stop ir state
                           )
                           (Map.lookup "player-name" processed)
                           (Map.lookup "game-name" processed)
                           (Map.lookup "public" processed))

gameJoinHandler :: String -> SimpleStetsonHandler (Maybe RunningGame)
gameJoinHandler gameId =
  Rest.handler (\req -> do
    maybeGame <- PureRunningGameList.findById gameId
    Rest.initResult req maybeGame)
    # Rest.allowedMethods (\req state -> Rest.result (Stetson.POST :  Stetson.HEAD : Stetson.GET : Stetson.OPTIONS : nil) req state)
    # Rest.resourceExists (\req state -> Rest.result (isJust state) req state)
    # Rest.contentTypesProvided (\req state -> Rest.result (jsonWriter : nil) req state)
    # Rest.contentTypesAccepted (\req state -> Rest.result ((tuple2 "application/x-www-form-urlencoded" acceptForm) : nil) req state)
    where
          acceptForm = (\req state -> do
                       UrlEncodedBody kvs req2  <- readUrlEncodedBody req
                       let processed = fromFoldable $ map (uncurry2 Tuple) kvs
                       fromMaybe (Rest.result false (setBody "unable to comply" req) state) $
                         (\playerName -> do
                               Log.info Log.Web "Attempt to join game" { playerName }
                               req2 <- replyWithoutBody (StatusCode 302) (ErlMap.fromFoldable [ (Tuple "Location" $ ServerRoutes.routeUrl ServerRoutes.GamePlay) ]) 
                                        $ setCookie "game_id" gameId $ setCookie "player-name" playerName req
                               Rest.stop req2 state
                           ) <$>
                           (Map.lookup "player-name" processed)
                       )



jsonWriter :: forall a. WriteForeign a => Tuple2 String (Req -> a -> (Effect (RestResult String a)))
jsonWriter = tuple2 "application/json" (\req state -> Rest.result (writeJSON state) req state)
