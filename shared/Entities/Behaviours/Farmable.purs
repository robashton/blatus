module Blatus.Entities.Behaviours.Farmable where

import Prelude
import Blatus.Entities (CollectableArgs)
import Data.Exists (Exists, mkExists)
import Data.Maybe (Maybe)
import Data.Symbol (SProxy(..))
import Data.Variant (Variant, default, inj, onMatch)
import Debug (spy)
import Sisy.BuiltIn.Extensions.Collider (vectorBetween)
import Sisy.Math (Point, scalePoint)
import Sisy.Runtime.Behaviour as B
import Sisy.Runtime.Entity (EntityBehaviour(..), EntityId(..))

init ::
  forall entity cmd ev.
  Config ->
  Exists (EntityBehaviour (Command cmd) (Event ev) (State entity))
init cfg =
  mkExists
    $ EntityBehaviour
        { state: { totalDamage: 0.0, spawned: 0 }
        , handleCommand:
            \command s ->
              onMatch { damage: \ev -> onDamage ev s }
                (default (pure s))
                command
        }
  where
  onDamage ev@{ amount } state@{ totalDamage, spawned }
    | amount + totalDamage > cfg.dropEvery = do
      entity <- B.entity
      B.raiseEvent
        $ collectableSpawned
            { id: entity.id <> EntityId (show spawned)
            , location: entity.location
            , velocity: scalePoint 0.5 $ vectorBetween entity.location ev.location
            , args: cfg.drop
            }
      pure $ state { totalDamage = 0.0, spawned = spawned + 1 }
    | otherwise = pure $ state { totalDamage = state.totalDamage + amount }

type Config
  = { dropEvery :: Number
    , drop :: CollectableArgs
    }

type State :: forall k. k -> k
type State r
  = ( | r )

type Command r
  = ( damage :: { amount :: Number, location :: Point, source :: Maybe EntityId }
    | r
    )

type Event r
  = ( collectableSpawned :: CollectableSpawned
    | r
    )

type CollectableSpawned
  = { id :: EntityId
    , location :: Point
    , velocity :: Point
    , args :: CollectableArgs
    }

collectableSpawned :: forall r. CollectableSpawned -> Variant (Event r)
collectableSpawned = inj (SProxy :: SProxy "collectableSpawned")
