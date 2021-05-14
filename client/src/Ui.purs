module Blatus.Client.Ui where

import Prelude
import Blatus.Client.Rendering as Rendering
import Blatus.Comms (ClientMsg(..), ServerMsg(..))
import Blatus.Entities.Tank as Tank
import Blatus.Main as Main
import Blatus.Types (EntityCommand)
import Control.Apply (lift2)
import Control.Monad.Except (runExcept)
import Data.DateTime.Instant as Instant
import Data.Either (either)
import Data.Foldable (foldl, for_)
import Data.Map (lookup) as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (unwrap, wrap)
import Data.Symbol (SProxy(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (traverse)
import Data.Tuple (fst)
import Data.Variant (Variant, expand, inj)
import Effect (Effect)
import Effect.Now as Now
import Foreign (readString)
import Signal (Signal, dropRepeats, foldp, runSignal, sampleOn)
import Signal as Signal
import Signal.Channel as Channel
import Signal.DOM (keyPressed)
import Signal.Time (every, second)
import Simple.JSON (readJSON, writeJSON)
import Sisy.Runtime.Entity (EntityId(..))
import Sisy.Runtime.Scene (entityById)
import Sisy.Types (empty)
import Web.DOM.Document as Document
import Web.DOM.Element as Element
import Web.DOM.Node as Node
import Web.DOM.NodeList as NodeList
import Web.DOM.ParentNode (QuerySelector(..), querySelector)
import Web.Event.EventTarget as EET
import Web.Event.EventTarget as ET
import Web.HTML as HTML
import Web.HTML.Event.EventTypes as ETS
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Location as Location
import Web.HTML.Window as Window
import Web.Socket.Event.EventTypes as WSET
import Web.Socket.Event.MessageEvent as ME
import Web.Socket.ReadyState as RS
import Web.Socket.WebSocket as WS


gameInfoSelector :: QuerySelector
gameInfoSelector = QuerySelector ("#game-info")

playerListSelector :: QuerySelector
playerListSelector = QuerySelector ("#player-list")

latencyInfoSelector :: QuerySelector
latencyInfoSelector = QuerySelector ("#latency-info")

gameMessageSelector :: QuerySelector
gameMessageSelector = QuerySelector ("#game-message")

healthSelector :: QuerySelector
healthSelector = QuerySelector ("#health")

shieldSelector :: QuerySelector
shieldSelector = QuerySelector ("#shield")

rockSelector :: QuerySelector
rockSelector = QuerySelector ("#rock")


init :: Document -> Effect Unit
init document = do 
  gameInfoElement <- querySelector gameInfoSelector $ Document.toParentNode document
  latencyInfoElement <- querySelector latencyInfoSelector $ Document.toParentNode document
  playerListElement <- querySelector playerListSelector $ Document.toParentNode document
  gameMessageElement <- querySelector gameMessageSelector $ Document.toParentNode document
  healthElement <- querySelector healthSelector $ Document.toParentNode document
  shieldElement <- querySelector shieldSelector $ Document.toParentNode document
  rockElement <- querySelector rockSelector $ Document.toParentNode document
  runSignal $ ( \lc -> do
              -- Update the display
              _ <-
                fromMaybe (pure unit)
                $ lift2
                        ( \element { gameUrl } -> do
                           Element.setAttribute "href" gameUrl element
                           Node.setTextContent gameUrl $ Element.toNode element
                           )
                           gameInfoElement
                           lc.info
                           _ <-
                             maybe (pure unit)
                    ( \element -> do
                    if lc.hasError then
                      Node.setTextContent "Error: Not connected" $ Element.toNode element
                      else
                      Node.setTextContent ("Connected (" <> (show (lc.tickLatency * 33)) <> "ms)") $ Element.toNode element
                      )
                      latencyInfoElement
                      _ <-
                        fromMaybe (pure unit)
                        $ lift2
                        ( \element { playerId } ->
                          if lc.hasError then
                            Node.setTextContent "Server disconnected, try refreshing or switch games" $ Element.toNode element
                            else case Main.pendingSpawn playerId lc.game of
                                      Nothing -> Node.setTextContent "" $ Element.toNode element
                                      Just ticks -> Node.setTextContent ("Waiting " <> (show (ticks `div` 30)) <> " seconds to respawn") $ Element.toNode element
                                      )
                                      gameMessageElement
                                      lc.info
                                      _ <-
                                        fromMaybe (pure unit)
                                        $ lift2
                        ( \element { playerId } ->
                          maybe (pure unit)
                              ( \player -> do
                              let
                                  percentage = show $ (player.health / Tank.maxHealth) * 100.0
                                  Node.setTextContent percentage $ Element.toNode element
                                  )
                                  $ entityById playerId lc.game.scene
                                  )
                                  healthElement
                                  lc.info
                                  _ <-
                                    fromMaybe (pure unit)
                                    $ lift2
                        ( \element { playerId } ->
                          maybe (pure unit)
                              ( \player -> do
                              let
                                  percentage = show $ (player.shield / Tank.maxShield) * 100.0
                                  Node.setTextContent percentage $ Element.toNode element
                                  )
                                  $ entityById playerId lc.game.scene
                                  )
                                  shieldElement
                                  lc.info
                                  _ <-
                                    fromMaybe (pure unit)
                                    $ lift2
                        ( \element { playerId } ->
                          maybe (pure unit)
                              ( \player -> do
                              let
                                  amount = show $ player.availableRock
                                  Node.setTextContent amount $ Element.toNode element
                                  )
                                  $ Map.lookup playerId lc.game.players
                                  )
                                  rockElement
                                  lc.info
                                  _ <-
                                    fromMaybe (pure unit)
                                    $ lift2
                        ( \element { playerId } -> do
                           let
                               node = Element.toNode element
                               existingChildren <- NodeList.toArray =<< Node.childNodes node
                            _ <- traverse (\child -> Node.removeChild child node) $ existingChildren
                            _ <-
                              traverse
                                ( \player -> do
                                li <- Element.toNode <$> Document.createElement "li" document
                                Node.setTextContent (unwrap playerId <> ": " <> (show player.score)) li
                                Node.appendChild li node
                                )
                                $ lc.game.players
                                pure unit
                                )
                                playerListElement
                                lc.info
                                pure unit
                                )
                                <$> sampleOn uiUpdateSignal gameStateSignal
