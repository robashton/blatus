module Sisy.BuiltIn.Behaviours.Farmable where

import Prelude
import Blatus.Entities.Types (CollectableArgs)
import Data.Exists (Exists, mkExists)
import Data.Maybe (Maybe)
import Data.Symbol (SProxy(..))
import Data.Variant (Variant, default, inj, onMatch)
import Sisy.Math (Point)
import Sisy.Runtime.Behaviour as B
import Sisy.Runtime.Entity (EntityBehaviour(..), EntityId)

init ::
  forall entity cmd ev.
  Exists (EntityBehaviour (Command cmd) (Event ev) (State entity))
init =
  mkExists
    $ EntityBehaviour
        { state: {}
        , handleCommand:
            \command s ->
              onMatch
                { damage:
                    \damage -> do
                      pure s
                }
                (default (pure s))
                command
        }

type State r
  = (
    | r
    )

type Command r
  = ( damage :: { amount :: Number, source :: Maybe EntityId }
    | r
    )

type Event r
  = ( collectableSpawned :: CollectableSpawned
    | r
    )

type CollectableSpawned
  = { location :: Point
    , args :: CollectableArgs
    }

collectableSpawned :: forall r. CollectableSpawned -> Variant (Event r)
collectableSpawned = inj (SProxy :: SProxy "collectableSpawned")
