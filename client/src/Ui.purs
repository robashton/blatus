module Blatus.Client.Ui where

import Prelude
import Blatus.Entities.Tank as Tank
import Blatus.Main as Main
import Data.Map (lookup) as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.Traversable (traverse)
import Effect (Effect)
import Signal (Signal, runSignal, sampleOn)
import Signal as Signal
import Signal.Time (every, second)
import Sisy.Runtime.Entity (EntityId)
import Sisy.Runtime.Scene (entityById)
import Web.DOM.Document as Document
import Web.DOM.Element as Element
import Web.DOM.Node as Node
import Web.DOM.NodeList as NodeList
import Web.DOM.ParentNode (QuerySelector(..), querySelector)
import Web.HTML as HTML
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window as Window

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

uiUpdateSignal :: Signal Unit
uiUpdateSignal = sampleOn (every $ second * 0.3) $ Signal.constant unit

type Config
  = { playerId :: EntityId
    , gameUrl :: String
    }

type Input
  = { game :: Main.State
    , error :: Maybe String
    , tickLatency :: Int
    }

init :: Config -> Signal Input -> Effect Unit
init { gameUrl, playerId } inputSignal = do
  window <- HTML.window
  document <- HTMLDocument.toDocument <$> Window.document window
  gameInfoElement <- querySelector gameInfoSelector $ Document.toParentNode document
  latencyInfoElement <- querySelector latencyInfoSelector $ Document.toParentNode document
  playerListElement <- querySelector playerListSelector $ Document.toParentNode document
  gameMessageElement <- querySelector gameMessageSelector $ Document.toParentNode document
  healthElement <- querySelector healthSelector $ Document.toParentNode document
  shieldElement <- querySelector shieldSelector $ Document.toParentNode document
  rockElement <- querySelector rockSelector $ Document.toParentNode document
  runSignal
    $ ( \lc -> do
          _ <-
            maybe (pure unit)
              ( \element -> do
                  Element.setAttribute "href" gameUrl element
                  Node.setTextContent gameUrl $ Element.toNode element
              )
              gameInfoElement
          _ <-
            maybe (pure unit)
              ( \element -> do
                  case lc.error of
                    Just err -> Node.setTextContent err $ Element.toNode element
                    Nothing -> Node.setTextContent ("Connected (" <> (show (lc.tickLatency * 33)) <> "ms)") $ Element.toNode element
              )
              latencyInfoElement
          _ <-
            maybe (pure unit)
              ( \element -> case lc.error of
                  Just _ -> Node.setTextContent "Server disconnected, try refreshing or switch games" $ Element.toNode element
                  Nothing -> case Main.pendingSpawn playerId lc.game of
                    Nothing -> Node.setTextContent "" $ Element.toNode element
                    Just ticks -> Node.setTextContent ("Waiting " <> (show (ticks `div` 30)) <> " seconds to respawn") $ Element.toNode element
              )
              gameMessageElement
          _ <-
            maybe (pure unit)
              ( \element ->
                  maybe (pure unit)
                    ( \player -> do
                        let
                          percentage = show $ (player.health / Tank.maxHealth) * 100.0
                        Node.setTextContent percentage $ Element.toNode element
                    )
                    $ entityById playerId lc.game.scene
              )
              healthElement
          _ <-
            maybe (pure unit)
              ( \element ->
                  maybe (pure unit)
                    ( \player -> do
                        let
                          percentage = show $ (player.shield / Tank.maxShield) * 100.0
                        Node.setTextContent percentage $ Element.toNode element
                    )
                    $ entityById playerId lc.game.scene
              )
              shieldElement
          _ <-
            maybe (pure unit)
              ( \element ->
                  maybe (pure unit)
                    ( \player -> do
                        let
                          amount = show $ player.availableRock
                        Node.setTextContent amount $ Element.toNode element
                    )
                    $ Map.lookup playerId lc.game.players
              )
              rockElement
          _ <-
            maybe (pure unit)
              ( \element -> do
                  let
                    node = Element.toNode element
                  existingChildren <- NodeList.toArray =<< Node.childNodes node
                  _ <- traverse (\child -> Node.removeChild child node) $ existingChildren
                  _ <-
                    traverse
                      ( \player -> do
                          li <- Element.toNode <$> Document.createElement "li" document
                          Node.setTextContent ((unwrap player.id) <> ": " <> (show player.score)) li
                          Node.appendChild li node
                      )
                      $ lc.game.players
                  pure unit
              )
              playerListElement
          pure unit
      )
    <$> sampleOn uiUpdateSignal inputSignal
