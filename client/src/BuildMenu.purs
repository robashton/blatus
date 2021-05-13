module Blatus.Client.BuildMenu where

import Prelude
import Data.Maybe (fromJust)
import Effect (Effect)
import Partial.Unsafe (unsafePartial)
import Sisy.Math (Point)
import Web.DOM (Element, ParentNode)
import Web.DOM.Element (QuerySelector)
import Web.DOM.Element as Element
import Web.DOM.ParentNode (QuerySelector(..), querySelector)

buildMenuSelector :: QuerySelector
buildMenuSelector = QuerySelector ("#build-menu")

type State
  = { buildMenu :: Element
    }

init :: ParentNode -> Effect State
init parent = do
  buildMenu <- (querySelector buildMenuSelector parent)
  pure
    { buildMenu: unsafePartial $ fromJust buildMenu
    }

show :: Point -> State -> Effect Unit
show location { buildMenu } = do
  Element.setClassName "enabled" buildMenu
  pure unit

hide :: Point -> State -> Effect Unit
hide location { buildMenu } = do 
  Element.setClassName "disabled" buildMenu
  pure unit
