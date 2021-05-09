module Blatus.Entities.Behaviours.ProvidesResource where

import Prelude
import Blatus.Types (CollectableType)
import Data.Exists (Exists, mkExists)
import Data.Maybe (Maybe)
import Data.Symbol (SProxy(..))
import Data.Variant (Variant, default, inj, onMatch)
import Sisy.Runtime.Behaviour as B
import Sisy.Runtime.Entity (EntityBehaviour(..), EntityId)

init ::
  forall entity cmd ev.
  CollectableType ->
  Exists (EntityBehaviour (Command cmd) (Event ev) (State entity))
init resource =
  mkExists
    $ EntityBehaviour
        { state: { shieldVisible: true }
        , handleCommand:
            \command s ->
              onMatch
                { impact:
                    \{ source } -> do
                      -- need to be able to get the scene here, or at least other entities
                      pure s
                }
                (default (pure s))
                command
        }

type State :: forall k. k -> k
type State r
  = ( | r )

type Command r
  = ( impact :: { amount :: Number, source :: EntityId }
    | r
    )

type Event r
  = ( entityDestroyed :: EntityDestroyed
    , resourceProvided :: { to :: EntityId, resource :: CollectableType }
    | r
    )

type EntityDestroyed
  = { entity :: EntityId, destroyer :: Maybe EntityId }

entityDestroyed :: forall r. EntityDestroyed -> Variant (Event r)
entityDestroyed = inj (SProxy :: SProxy "entityDestroyed")
