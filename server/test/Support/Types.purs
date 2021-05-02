module Test.Support.Types where

import Prelude
import Data.Exists (Exists, mkExists)
import Data.List (List(..))
import Data.Symbol (SProxy(..))
import Data.Variant (Variant, default, inj, onMatch)
import Sisy.Runtime.Behaviour (raiseEvent)
import Sisy.Runtime.Behaviour as B
import Sisy.Runtime.Entity (Entity, EntityBehaviour(..), EntityId)
import Sisy.Math (point)

type TestEvent
  = ( ticked :: EntityId )

ticked :: EntityId -> Variant TestEvent
ticked = inj (SProxy :: SProxy "ticked")

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
                        raiseEvent $ ticked id
                        pure state
                  }
                  (default (pure state))
                  cmd
            )
        }
