module Shared.ServerRoutes where

import Prelude hiding ((/))

import Data.Either (Either(..))
import Data.Generic.Rep (class Generic, NoArguments(..), Product(..))
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Profunctor (dimap)
import Routing.Duplex (RouteDuplex', as, path, rest, segment)
import Routing.Duplex as RouteDuplex
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/))

data TopRoute =
    Assets (Array String)
  | Art (Array String)
  | GameStart
  | GameJoinHtml String
  | GameJoin String
  | GameComms
  | GamePlay
  | Index 

derive instance genericTopRoute :: Generic TopRoute _

instance showTopRoute :: Show TopRoute where
  show = genericShow

-- TODO: Ask Nick about nesting these
topRoute :: RouteDuplex' TopRoute
topRoute = path "" $ sum
  { "Assets" : "assets" / rest
  , "Art" : "art" / rest
  , "GameStart" : "game" / noArgs 
  , "GameJoinHtml" : "game" / segment / ""
  , "GameJoin" : "game" / segment  / "join"
  , "GamePlay": "play" / noArgs
  , "GameComms":  "game" / "messaging" / noArgs
  , "Index": noArgs
  }

routeUrl :: TopRoute -> String
routeUrl = RouteDuplex.print topRoute

asNewtype :: forall a. Newtype a String => RouteDuplex' String -> RouteDuplex' a
asNewtype = as unwrap (pure <<< wrap)

ignoreRest :: RouteDuplex' (Array String) -> RouteDuplex' NoArguments
ignoreRest = dimap (const []) (const NoArguments)

segmentExcept :: String -> RouteDuplex' String 
segmentExcept s = as identity f $ segment
  where
  f x = if x == s then Left "matched except" else Right x
        

