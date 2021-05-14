module Blatus.Client.BuildMenu where

import Prelude
import Blatus.Main as Main
import Blatus.Types (Build, BuildTemplate(..), EntityCommand)
import Data.Maybe (Maybe(..), fromJust)
import Data.Traversable (traverse)
import Data.Variant (Variant)
import Effect (Effect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Partial.Unsafe (unsafeCrashWith, unsafePartial)
import Signal (Signal)
import Signal.Channel (Channel)
import Signal.Channel as Channel
import Sisy.Math (Point)
import Sisy.Runtime.Entity (EntityId)
import Web.DOM (Document, Element, Node, ParentNode)
import Web.DOM.ChildNode as ChildNode
import Web.DOM.Document as Document
import Web.DOM.Element as Element
import Web.DOM.HTMLCollection as HTMLCollection
import Web.DOM.Node as Node
import Web.DOM.ParentNode (QuerySelector(..), querySelector)

--buildMenuSelector :: QuerySelector
--buildMenuSelector = QuerySelector ("#build-menu")
--
--canvasSelector :: QuerySelector
--canvasSelector = QuerySelector ("#target")
--
--type Coordinate
--  = { x :: Int, y :: Int }
--
type State
  = Ref
      { buildMenu :: Element
      , document :: Document
      , template :: Node
      , commandChannel :: Channel (Maybe (Variant EntityCommand))
      , currentTemplate :: Maybe BuildTemplate
      }
--
--commandSignal :: State -> Effect (Signal (Maybe (Variant EntityCommand)))
--commandSignal ref = Channel.subscribe <$> (_.commandChannel <$> Ref.read ref)
init :: Document -> EntityId -> Signal (Main.State) -> Effect State
init document playerId gameStateSignal = unsafeCrashWith "Not implemented"

--  buildMenu <- querySelector buildMenuSelector $ Document.toParentNode document
--  canvasElement <- querySelector canvasSelector $ Document.toParentNode document
--  template <- Element.toNode <$> Document.createElement "div" document
--  commandChannel <- Channel.channel Nothing
--  menuChannel <- Channel.channel Nothing
--  menuListener <-
--    ET.eventListener
--      ( \ev -> do
--          Event.preventDefault ev
--          let
--            me = MouseEvent.fromEvent ev
--          case me of
--            Just e -> Channel.send buildChannel $ Just { x: MouseEvent.clientX e, y: MouseEvent.clientY e }
--            _ -> Channel.send buildChannel Nothing
--      )
--  runSignal
--    $ ( \input -> case input.build of
--          Nothing -> pure unit
--          Just l -> BuildMenu.display l buildMenu playerId input.game
--      )
--    <$> ({ game: _, build: _ } <~ (sampleOn buildSignal gameStateSignal) ~ buildSignal)
--  maybe (pure unit) (\element -> ET.addEventListener (EventType "contextmenu") buildListener true $ Element.toEventTarget element) canvasElement
--  Ref.new
--    { buildMenu: unsafePartial $ fromJust buildMenu
--    , document
--    , template
--    , commandChannel
--    , currentTemplate: Nothing
--    }
--
--display :: Coordinate -> State -> EntityId -> Main.State -> Effect Unit
--display location ref playerId game = do
--  { buildMenu, template } <- Ref.read ref
--  let
--    actions = Main.playerBuildActions playerId game
--
--    buildMenuNode = Element.toNode buildMenu
--  void $ traverse (Element.toChildNode >>> ChildNode.remove)
--    =<< HTMLCollection.toArray
--    =<< Element.getElementsByTagName "div" buildMenu
--  void
--    $ traverse
--        ( \a@{ template: BuildTemplate name } -> do
--            node <- Node.clone template
--            case (Element.fromNode node) of
--              Just e -> do
--                _ <- Element.setClassName ("build-option " <> name <> if a.available then " available" else " unavailable") e
--                _ <- Node.setTextContent name node
--                _ <- Element.setAttribute "title" a.description e
--                _ <- Node.appendChild node buildMenuNode
--                pure unit
--              Nothing -> pure unit
--        )
--        actions
--  Element.setClassName "enabled" buildMenu
--  Element.setAttribute "style"
--    ( ("left: " <> (show location.x <> "px;"))
--        <> ("top: " <> (show location.y <> "px;"))
--    )
--    buildMenu
--  pure unit
--
--hide :: Point -> State -> Effect Unit
--hide location ref = do
--  { buildMenu } <- Ref.read ref
--  Element.setClassName "disabled" buildMenu
--  pure unit
