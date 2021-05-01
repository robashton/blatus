module Pure.Behaviours.Regenerates where

import Prelude
import Data.Exists (Exists, mkExists)
import Math as Math
import Prim.Row as Row
import Pure.Behaviour as B
import Pure.Entity (EntityBehaviour(..))
import Pure.Types (EntityCommand(..), GameEvent(..))

type Config
  = { healthRegen :: Number
    , shieldRegen :: Number
    , healthDelay :: Int
    , shieldDelay :: Int
    , maxHealth :: Number
    , maxShield :: Number
    }

init ::
  forall entity.
  Config -> Exists (EntityBehaviour EntityCommand GameEvent (Required entity))
init cfg =
  mkExists
    $ EntityBehaviour
        { state: { shieldRegenCountdown: 0, healthRegenCountdown: 0 }
        , handleCommand:
            \command s -> case command of
              Damage _ -> do
                pure $ s { shieldRegenCountdown = cfg.shieldDelay, healthRegenCountdown = cfg.healthDelay }
              Tick -> do
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
              _ -> pure s
        }

type Required r
  = ( health :: Number
    , shield :: Number
    | r
    )
