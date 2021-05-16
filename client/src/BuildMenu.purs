module Blatus.Client.BuildMenu where

import Prelude
import Blatus.BuildMenu (BuildActionInfo)
import Blatus.Client.Camera (Camera)
import Blatus.Client.Camera as Camera
import Blatus.Main as Main
import Blatus.Types (Build, BuildTemplate(..), EntityCommand)
import Control.Apply (lift2)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromJust, fromMaybe, isJust, maybe)
import Data.Traversable (traverse)
import Data.Variant (Variant, inj)
import Debug (spy)
import Effect (Effect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Graphics.Canvas as Canvas
import Partial.Unsafe (unsafeCrashWith, unsafePartial)
import Signal (Signal, filter, filterMap, get, sampleOn, (~), map2)
import Signal.Channel (Channel)
import Signal.Channel as Channel
import Signal.Effect (foldEffect)
import Sisy.BuiltIn (impact)
import Sisy.Math (Point, origin)
import Sisy.Runtime.Behaviour (entity)
import Sisy.Runtime.Entity (EntityId(..))
import Sisy.Runtime.Scene as Scene
import Web.DOM (Document, Element, Node, ParentNode)
import Web.DOM.ChildNode as ChildNode
import Web.DOM.Document as Document
import Web.DOM.Element as Element
import Web.DOM.HTMLCollection as HTMLCollection
import Web.DOM.Node as Node
import Web.DOM.ParentNode (QuerySelector(..), querySelector)
import Web.Event.Event (EventType(..))
import Web.Event.Event as Event
import Web.Event.EventTarget (EventListener)
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
    , currentTemplate :: Maybe BuildActionInfo
    , open :: Boolean
    , mouse :: Point
    }

data BuildMenuMessage
  = ToggleMenu
    { game :: Main.State
    , build :: Coordinate
    }
  | SelectMenuItem
    { game :: Main.State
    , item :: BuildTemplate
    }
  | SendBuildCommand
  | UpdateMouse Point
  | None

type Input
  = { game :: Main.State
    , camera :: Camera
    }

type Output
  = { game :: Main.State
    , camera :: Camera
    }

type Handle
  = { commands :: (Signal (Variant EntityCommand))
    , output :: Signal Output
    }

mkMenuListener :: Channel (Maybe Coordinate) -> Effect EventListener
mkMenuListener menuChannel =
  ET.eventListener
    ( \ev -> do
        Event.preventDefault ev
        let
          me = MouseEvent.fromEvent ev
        case me of
          Just e -> Channel.send menuChannel $ Just { x: MouseEvent.clientX e, y: MouseEvent.clientY e }
          _ -> Channel.send menuChannel Nothing
    )

mkTrackListener :: { width :: Number, height :: Number } -> Channel Point -> Effect EventListener
mkTrackListener canvas trackChannel =
  ET.eventListener
    ( \ev -> do
        Event.preventDefault ev
        let
          me = MouseEvent.fromEvent ev

          canvasElement = Element.fromEventTarget =<< (Event.target ev)
        fromMaybe (pure unit)
          $ lift2
              ( \mouseEvent target -> do
                  elementX <- Element.clientLeft target
                  elementY <- Element.clientTop target
                  realWidth <- Element.clientWidth target
                  realHeight <- Element.clientHeight target
                  let
                    elementLocation = { x: elementX, y: elementY }

                    mouseLocation = { x: toNumber $ MouseEvent.clientX mouseEvent, y: toNumber $ MouseEvent.clientY mouseEvent }

                    scaleX = canvas.width / realWidth

                    scaleY = canvas.height / realHeight

                    scaledLocation =
                      { x: (mouseLocation.x - elementLocation.x) * scaleX
                      , y: (mouseLocation.y - elementLocation.y) * scaleY
                      }
                  Channel.send trackChannel scaledLocation
              )
              me
              canvasElement
    --        case me of
    --          Just e -> Channel.send trackChannel $ Just { x: MouseEvent.clientX e, y: MouseEvent.clientY e }
    --          _ -> pure unit
    )

mkSelectListener :: Channel (Maybe String) -> Effect EventListener
mkSelectListener selectChannel =
  ET.eventListener
    ( \ev -> do
        Event.preventDefault ev
        let
          targetElement = Element.fromEventTarget =<< (Event.target ev)
        template <- maybe (pure Nothing) (Element.getAttribute "data-template") targetElement
        Channel.send selectChannel template
    )

mkBuildListener :: Channel Boolean -> Effect EventListener
mkBuildListener buildChannel =
  ET.eventListener
    ( \ev -> do
        Event.preventDefault ev
        Channel.send buildChannel true
    )

init :: EntityId -> Signal Input -> Effect Handle
init playerId gameStateSignal = do
  window <- HTML.window
  document <- HTMLDocument.toDocument <$> Window.document window
  buildMenu <- unsafePartial fromJust <$> (querySelector buildMenuSelector $ Document.toParentNode document)
  canvasElement <- unsafePartial fromJust <$> (querySelector canvasSelector $ Document.toParentNode document)
  canvas <- (unsafePartial fromJust) <$> Canvas.getCanvasElementById "target"
  canvasWidth <- Canvas.getCanvasWidth canvas
  canvasHeight <- Canvas.getCanvasHeight canvas
  template <- Element.toNode <$> Document.createElement "div" document
  commandChannel <- Channel.channel Nothing
  menuChannel <- Channel.channel Nothing
  selectChannel <- Channel.channel Nothing
  buildChannel <- Channel.channel false
  trackChannel <- Channel.channel { x: 0.0, y: 0.0 }
  menuListener <- mkMenuListener menuChannel
  trackListener <- mkTrackListener { width: canvasWidth, height: canvasHeight } trackChannel
  selectListener <- mkSelectListener selectChannel
  buildListener <- mkBuildListener buildChannel
  void $ ET.addEventListener (EventType "contextmenu") menuListener true $ Element.toEventTarget canvasElement
  void $ ET.addEventListener (EventType "mousemove") trackListener true $ Element.toEventTarget canvasElement
  void $ ET.addEventListener (EventType "click") buildListener true $ Element.toEventTarget canvasElement
  void $ ET.addEventListener (EventType "click") selectListener true $ Element.toEventTarget buildMenu
  let
    menuSignal = Channel.subscribe menuChannel

    selectSignal = Channel.subscribe selectChannel

    initialState =
      { buildMenu
      , document
      , template
      , commandChannel
      , currentTemplate: Nothing
      , open: false
      , mouse: origin
      }

    menuToggleSignal =
      sampleOn menuSignal
        $ ( \game build -> ToggleMenu <$> { game, build: _ } <$> build
          )
        <$> _.game
        <$> gameStateSignal
        <*> menuSignal

    menuSelectSignal =
      sampleOn selectSignal
        $ ( \game item ->
              SelectMenuItem <$> { game, item: _ } <$> BuildTemplate <$> item
          )
        <$> _.game
        <$> gameStateSignal
        <*> selectSignal
  state <-
    foldEffect
      ( \ev state -> case ev of
          ToggleMenu cmd -> do
            if state.open then
              hide state
            else
              display cmd.build state playerId cmd.game
            pure $ state { open = not state.open, currentTemplate = Nothing }
          SelectMenuItem cmd -> do
            case (Main.buildAction cmd.item playerId cmd.game) of
              Nothing -> pure state
              Just action ->
                if action.available then do
                  hide state
                  pure $ state { open = false, currentTemplate = Just action }
                else
                  pure state
          UpdateMouse p -> pure $ state { mouse = p }
          SendBuildCommand -> case state.currentTemplate of
            Nothing -> pure state
            Just template -> pure state -- Channel.send commandChannel $ build { location: state.mouse
          None -> pure state
      )
      initialState
      $ ( filterMap identity None
            $ menuToggleSignal
            <> menuSelectSignal
        )
      <> (UpdateMouse <$> Channel.subscribe trackChannel)
      <> ((\a -> if a then SendBuildCommand else None) <$> Channel.subscribe buildChannel)
  let
    output =
      map2
        ( \{ game, camera } { currentTemplate, mouse } -> case currentTemplate of
            Nothing -> { game, camera }
            Just t ->
              let
                world = spy "world" $ Camera.canvasToWorld camera mouse

                entity = t.build (EntityId "build-template") world
              in
                { game:
                    game
                      { scene = Scene.addEntity entity game.scene
                      }
                , camera
                }
        )
        gameStateSignal
        (sampleOn gameStateSignal state)
  pure
    { commands: filterMap identity unusedCommand $ Channel.subscribe commandChannel
    , output
    }

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
                _ <- Element.setAttribute "data-template" name e
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
