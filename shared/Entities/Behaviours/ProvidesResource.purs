module Blatus.Entities.Behaviours.ProvidesResource where

import Prelude
import Blatus.Entities (CollectableType, EntityClass(..))
import Data.Exists (Exists, mkExists)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Variant (Variant, default, inj, onMatch)
import Debug (spy)
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
                      scene <- B.scene
                      entity <- B.entity
                      let
                        collider = scene.entityById source
                      case (_.class <$> collider) of
                        Just Tank -> do
                          B.raiseEvent $ resourceProvided { to: source, resource }
                          B.raiseEvent $ entityDestroyed { entity: entity.id, destroyer: Just source }
                          pure s
                        _ -> pure s
                }
                (default (pure s))
                command
        }

type State r
  = ( class :: EntityClass | r )

type Command r
  = ( impact :: { force :: Number, source :: EntityId }
    | r
    )

type Event r
  = ( entityDestroyed :: EntityDestroyed
    , resourceProvided :: ResourceProvided
    | r
    )

type EntityDestroyed
  = { entity :: EntityId, destroyer :: Maybe EntityId }

type ResourceProvided
  = { to :: EntityId, resource :: CollectableType }

entityDestroyed :: forall r. EntityDestroyed -> Variant (Event r)
entityDestroyed = inj (SProxy :: SProxy "entityDestroyed")

resourceProvided :: forall r. ResourceProvided -> Variant (Event r)
resourceProvided = inj (SProxy :: SProxy "resourceProvided")
