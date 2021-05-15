module Blatus.Client.BuildMenu where

import Prelude
import Blatus.Main as Main
import Blatus.Types (Build, BuildTemplate(..), EntityCommand)
import Data.Maybe (Maybe(..), fromJust, isJust, maybe)
import Data.Traversable (traverse)
import Data.Variant (Variant, inj)
import Debug (spy)
import Effect (Effect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Partial.Unsafe (unsafeCrashWith, unsafePartial)
import Signal (Signal, filter, filterMap, sampleOn, (~))
import Signal.Channel (Channel)
import Signal.Channel as Channel
import Signal.Effect (foldEffect)
import Sisy.BuiltIn (impact)
import Sisy.Math (Point)
import Sisy.Runtime.Behaviour (entity)
import Sisy.Runtime.Entity (EntityId(..))
import Sisy.Runtime.Scene (tickCmd)
import Web.DOM (Document, Element, Node, ParentNode)
import Web.DOM.ChildNode as ChildNode
import Web.DOM.Document as Document
import Web.DOM.Element as Element
import Web.DOM.HTMLCollection as HTMLCollection
import Web.DOM.Node as Node
import Web.DOM.ParentNode (QuerySelector(..), querySelector)
import Web.Event.Event (EventType(..))
import Web.Event.Event as Event
import Web.Event.EventTarget as ET
import Web.HTML as HTML
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window as Window
import Web.UIEvent.MouseEvent as MouseEvent

buildMenuSelector :: QuerySelector
buildMenuSelector = QuerySelector ("#build-menu")

--
canvasSelector :: QuerySelector
canvasSelector = QuerySelector ("#target")

type Coordinate
  = { x :: Int, y :: Int }

type State
  = { buildMenu :: Element
    , document :: Document
    , template :: Node
    , commandChannel :: Channel (Maybe (Variant EntityCommand))
    , currentTemplate :: Maybe BuildTemplate
    , open :: Boolean
    }

data BuildMenuMessage
  = ToggleMenu
    { game :: Main.State
    , build :: Coordinate
    }
  | None

init :: EntityId -> Signal (Main.State) -> Effect (Signal (Variant EntityCommand))
init playerId gameStateSignal = do
  window <- HTML.window
  document <- HTMLDocument.toDocument <$> Window.document window
  buildMenu <- querySelector buildMenuSelector $ Document.toParentNode document
  canvasElement <- querySelector canvasSelector $ Document.toParentNode document
  template <- Element.toNode <$> Document.createElement "div" document
  commandChannel <- Channel.channel Nothing
  menuChannel <- Channel.channel Nothing
  menuListener <-
    ET.eventListener
      ( \ev -> do
          Event.preventDefault ev
          let
            me = MouseEvent.fromEvent ev
          case me of
            Just e -> Channel.send menuChannel $ Just { x: MouseEvent.clientX e, y: MouseEvent.clientY e }
            _ -> Channel.send menuChannel Nothing
      )
  void $ maybe (pure unit) (\element -> ET.addEventListener (EventType "contextmenu") menuListener true $ Element.toEventTarget element) canvasElement
  let
    menuSignal = Channel.subscribe menuChannel

    initialState =
      { buildMenu: unsafePartial $ fromJust buildMenu
      , document
      , template
      , commandChannel
      , currentTemplate: Nothing
      , open: false
      }

    menuToggleSignal =
      sampleOn menuSignal
        $ ( \game build ->
              ToggleMenu <$> { game, build: _ } <$> build
          )
        <$> gameStateSignal
        <*> menuSignal
  void
    $ foldEffect
        ( \ev state -> case ev of
            ToggleMenu cmd -> do
              if state.open then
                hide state
              else
                display cmd.build state playerId cmd.game
              pure $ state { open = not state.open }
            None -> pure state
        )
        initialState
    $ filterMap identity None
    $ menuToggleSignal
  pure $ filterMap identity unusedCommand $ Channel.subscribe commandChannel

unusedCommand :: Variant EntityCommand
unusedCommand = impact { force: 0.0, source: EntityId "" }

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
                _ <- Element.setClassName ("build-option " <> name <> if a.available then " available" else " unavailable") e
                _ <- Node.setTextContent name node
                _ <- Element.setAttribute "title" a.description e
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

hide :: State -> Effect Unit
hide { buildMenu } = do
  Element.setClassName "disabled" buildMenu
  pure unit
