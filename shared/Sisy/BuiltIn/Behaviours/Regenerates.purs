module Sisy.BuiltIn.Behaviours.Regenerates where

import Prelude
import Data.Exists (Exists, mkExists)
import Data.Maybe (Maybe)
import Data.Variant (default, onMatch)
import Math as Math
import Sisy.BuiltIn (Damage)
import Sisy.Math (Point)
import Sisy.Runtime.Behaviour as B
import Sisy.Runtime.Entity (EntityBehaviour(..), EntityId)

type Config
  = { healthRegen :: Number
    , shieldRegen :: Number
    , healthDelay :: Int
    , shieldDelay :: Int
    , maxHealth :: Number
    , maxShield :: Number
    }

init ::
  forall entity cmd ev.
  Config -> Exists (EntityBehaviour (Command cmd) ev (Required entity))
init cfg =
  mkExists
    $ EntityBehaviour
        { state: { shieldRegenCountdown: 0, healthRegenCountdown: 0 }
        , handleCommand:
            \command s ->
              onMatch
                { damage:
                    \_ -> do
                      pure $ s { shieldRegenCountdown = cfg.shieldDelay, healthRegenCountdown = cfg.healthDelay }
                , tick:
                    \_ -> do
                      e' <- B.entity
                      _ <-
                        if s.shieldRegenCountdown <= 0 && e'.shield < cfg.maxShield then
                          B.updateEntity \e -> e { shield = Math.min (e.shield + cfg.shieldRegen) cfg.maxShield }
                        else
                          pure unit
                      _ <-
                        if s.healthRegenCountdown <= 0 && e'.health < cfg.maxHealth then
                          B.updateEntity \e -> e { health = Math.min (e.health + cfg.healthRegen) cfg.maxHealth }
                        else
                          pure unit
                      pure $ s { shieldRegenCountdown = s.shieldRegenCountdown - 1, healthRegenCountdown = s.healthRegenCountdown - 1 }
                }
                (default (pure s))
                command
        }

type Required r
  = ( health :: Number
    , shield :: Number
    | r
    )

type Command cmd
  = ( damage :: Damage
    | cmd
    )
