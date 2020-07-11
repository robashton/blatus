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


data Route =
    Assets (Array String)
  | Art (Array String)
  | Index (Array String)
  | Game (Array String)

derive instance genericRoute :: Generic Route _

instance showRoute :: Show Route where
  show = genericShow

asNewtype :: forall a. Newtype a String => RouteDuplex' String -> RouteDuplex' a
asNewtype = as unwrap (pure <<< wrap)

ignoreRest :: RouteDuplex' (Array String) -> RouteDuplex' NoArguments
ignoreRest = dimap (const []) (const NoArguments)

segmentExcept :: String -> RouteDuplex' String 
segmentExcept s = as identity f $ segment
  where
  f x = if x == s then Left "matched except" else Right x
        

apiRoute :: RouteDuplex' Route
apiRoute = path "" $ sum
  { "Assets" : "assets" / rest
  , "Art" : "art" / rest
  , "Index": "index.html" / rest
  , "Game" : "game.html" / rest
  }

routeUrl :: Route -> String
routeUrl = RouteDuplex.print apiRoute
