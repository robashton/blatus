module Test.Support.Types where

import Prelude
import Data.Exists (Exists, mkExists)
import Data.Generic.Rep (class Generic)
import Data.List (List(..))
import Data.Show.Generic (genericShow)
import Debug.Trace (spy)
import GenericJSON (writeTaggedSumRep, taggedSumRep)
import Pure.Behaviour (raiseEvent)
import Pure.Behaviour as B
import Pure.Entity (Entity, EntityBehaviour(..),  EntityId(..))
import Pure.Math (point)
import Simple.JSON (class ReadForeign, class WriteForeign)

data TestCommand
  = Tick

data TestEvent
  = Ticked EntityId

derive instance genericTestCommand :: Generic TestCommand _

instance showTestCommand :: Show TestCommand where
  show = genericShow

instance writeForeignTestCommand :: WriteForeign TestCommand where
  writeImpl = writeTaggedSumRep

instance readForeignTestCommand :: ReadForeign TestCommand where
  readImpl = taggedSumRep

derive instance eqTestCommand :: Eq TestCommand

derive instance genericTestEvent :: Generic TestEvent _

instance showTestEvent :: Show TestEvent where
  show = genericShow

instance writeForeignTestEvent :: WriteForeign TestEvent where
  writeImpl = writeTaggedSumRep

instance readForeignTestEvent :: ReadForeign TestEvent where
  readImpl = taggedSumRep

derive instance eqTestEvent :: Eq TestEvent

emptyEntity :: EntityId -> Entity TestCommand TestEvent ()
emptyEntity =
  { id: _
  , location: point 0.0 0.0
  , width: 5.0
  , height: 5.0
  , velocity: point 0.0 0.0
  , friction: 1.0
  , rotation: 0.0
  , mass: 20.0
  , behaviour: Nil
  , renderables: Nil
  }

tickEcho :: forall entity. Exists (EntityBehaviour TestCommand TestEvent entity)
tickEcho =
  mkExists
    $ EntityBehaviour
        { state: {}
        , handleCommand:
            ( \cmd state ->
                ( case cmd of
                    Tick -> do
                      id <- B.id
                      raiseEvent $ Ticked id
                      pure state
                )
            )
        }
