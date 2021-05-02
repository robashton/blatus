module Pure.Behaviours.NetworkSync where

import Prelude
import Data.Exists (Exists, mkExists)
import Data.Variant (default, onMatch)
import Pure.Behaviour as B
import Pure.Entity (EntityBehaviour(..))
import Pure.Math (Point, lerp)

type ElasticConfig
  = { force :: Number }

init :: forall entity cmd ev. ElasticConfig -> Exists (EntityBehaviour (Command cmd) ev entity)
init c =
  mkExists
    $ EntityBehaviour
        { state:
            { force: c.force
            , location: { x: 0.0, y: 0.0 }
            , rotation: 0.0
            , velocity: { x: 0.0, y: 0.0 }
            , oldLocation: { x: 0.0, y: 0.0 }
            , oldRotation: 0.0
            , active: false
            }
        , handleCommand: handleCommand
        }
  where
  handleCommand command s = do
    e <- B.entity
    onMatch
      { updateServerState:
          \ss ->
            pure
              $ s
                  { oldLocation = e.location
                  , oldRotation = e.rotation
                  , location = ss.location
                  , rotation = ss.rotation
                  , active = true
                  }
      , tick:
          \_ ->
            if not s.active then
              pure s
            else do
              let
                targetRotation = s.rotation + (e.rotation - s.oldRotation)

                targetLocation = s.location + (e.location - s.oldLocation)

                newLocation = lerp e.location targetLocation s.force

                newRotation = e.rotation + s.force * (targetRotation - e.rotation)
              _ <-
                B.updateEntity
                  ( \entity ->
                      entity
                        { location = newLocation
                        , rotation = newRotation
                        }
                  )
              pure
                s
                  { rotation = targetRotation
                  , location = targetLocation
                  , oldRotation = newRotation
                  , oldLocation = newLocation
                  }
      }
      (default (pure s))
      command

type Command cmd
  = ( updateServerState ::
        { location :: Point
        , velocity :: Point
        , rotation :: Number
        }
    | cmd
    )
