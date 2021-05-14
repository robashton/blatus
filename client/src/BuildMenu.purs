module Blatus.Client.BuildMenu where

import Prelude
import Blatus.Main as Main
import Blatus.Types (BuildTemplate(..))
import Data.Maybe (Maybe(..), fromJust)
import Data.Traversable (traverse)
import Effect (Effect)
import Partial.Unsafe (unsafePartial)
import Sisy.Math (Point)
import Sisy.Runtime.Entity (EntityId)
import Web.DOM (Document, Element, Node, ParentNode)
import Web.DOM.ChildNode as ChildNode
import Web.DOM.Document as Document
import Web.DOM.Element as Element
import Web.DOM.HTMLCollection as HTMLCollection
import Web.DOM.Node as Node
import Web.DOM.ParentNode (QuerySelector(..), querySelector)

buildMenuSelector :: QuerySelector
buildMenuSelector = QuerySelector ("#build-menu")

type Coordinate
  = { x :: Int, y :: Int }

type State
  = { buildMenu :: Element
    , document :: Document
    , template :: Node
    }

init :: Document -> Effect State
init document = do
  buildMenu <- querySelector buildMenuSelector $ Document.toParentNode document
  template <- Element.toNode <$> Document.createElement "div" document
  pure
    { buildMenu: unsafePartial $ fromJust buildMenu
    , document
    , template
    }

display :: Coordinate -> State -> EntityId -> Main.State -> Effect Unit
display location { buildMenu, template } playerId game = do
  let
    actions = Main.playerBuildActions playerId game

    buildMenuNode = Element.toNode buildMenu
  void $ traverse (Element.toChildNode >>> ChildNode.remove)
    =<< HTMLCollection.toArray
    =<< Element.getElementsByTagName "div" buildMenu
  void
    $ traverse
        ( \a@{ template: BuildTemplate name } -> do
            node <- Node.clone template
            case (Element.fromNode node) of
              Just e -> do
                _ <- Element.setClassName ("build-option " <> name) e
                _ <- Node.setTextContent name node
                _ <- Node.appendChild node buildMenuNode
                pure unit
              Nothing -> pure unit
        )
        actions
  Element.setClassName "enabled" buildMenu
  Element.setAttribute "style"
    ( ("left: " <> (show location.x <> "px;"))
        <> ("top: " <> (show location.y <> "px;"))
    )
    buildMenu
  pure unit

hide :: Point -> State -> Effect Unit
hide location { buildMenu } = do
  Element.setClassName "disabled" buildMenu
  pure unit
