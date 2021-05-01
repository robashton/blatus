module Test.Support.Types where

import Prelude
import Data.Exists (Exists, mkExists)
import Data.Generic.Rep (class Generic)
import Data.List (List(..))
import Data.Show.Generic (genericShow)
import Data.Variant (default, onMatch)
import GenericJSON (writeTaggedSumRep, taggedSumRep)
import Pure.Behaviour (raiseEvent)
import Pure.Behaviour as B
import Pure.Entity (Entity, EntityBehaviour(..), EntityId(..))
import Pure.Math (point)
import Simple.JSON (class ReadForeign, class WriteForeign)

data TestEvent
  = Ticked EntityId

derive instance genericTestEvent :: Generic TestEvent _

instance showTestEvent :: Show TestEvent where
  show = genericShow

instance writeForeignTestEvent :: WriteForeign TestEvent where
  writeImpl = writeTaggedSumRep

instance readForeignTestEvent :: ReadForeign TestEvent where
  readImpl = taggedSumRep

derive instance eqTestEvent :: Eq TestEvent

emptyEntity :: EntityId -> Entity () TestEvent ()
emptyEntity =
  { id: _
  , location: point 0.0 0.0
  , width: 5.0
  , height: 5.0
  , rotation: 0.0
  , behaviour: Nil
  , renderables: Nil
  }

tickEcho :: forall entity. Exists (EntityBehaviour () TestEvent entity)
tickEcho =
  mkExists
    $ EntityBehaviour
        { state: {}
        , handleCommand:
            ( \cmd state ->
                onMatch
                  { tick:
                      \_ -> do
                        id <- B.id
                        raiseEvent $ Ticked id
                        pure state
                  }
                  (default (pure state))
                  cmd
            )
        }
