module Test.Support.Requests where

import Prelude
import Control.Apply (lift2)
import Data.Foldable (intercalate)
import Data.List (List, catMaybes)
import Data.Maybe (Maybe)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Erl.Atom (Atom)
import Erl.Data.Tuple (Tuple2)
import JSURI (encodeFormURLComponent)

foreign import requestPage :: String -> Effect (Tuple2 Atom (Maybe String))

foreign import postFormImpl :: String -> String -> Effect (Tuple2 Atom (Maybe String))

postForm :: String -> List (Tuple String String) -> Effect (Tuple2 Atom (Maybe String))
postForm url params = do
  let
    components :: List String
    components = catMaybes $ (\(Tuple k v) -> lift2 (\l r -> l <> "=" <> r) (encodeFormURLComponent k) (encodeFormURLComponent v)) <$> params

    form = intercalate "&" components
  postFormImpl url form
