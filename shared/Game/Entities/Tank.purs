module Pure.Entities.Tank where

import Prelude

import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Pure.Behaviours.BasicBitchPhysics as BasicBitchPhysics
import Pure.Entity (Entity, EntityClass(..), EntityId, HtmlColor(..), sprite)
import Pure.Math (Point)

import Pure.Behaviours.Damageable as Damageable
import Pure.Behaviours.FiresBullets as FiresBullets
import Pure.Behaviours.Driven as Driven
import Pure.Behaviours.NetworkSync as NetworkSync
import Pure.Behaviours.Regenerates as Regenerates

import Pure.Types (EntityCommand, GameEvent)


maxHealth :: Number
maxHealth = 100.0

maxShield :: Number
maxShield = 50.0

init :: EntityId -> Point -> Entity EntityCommand GameEvent
init id location = { id
                   , location
                   , class: Tank
                   , width: 25.0
                   , height: 25.0
                   , velocity: { x: 0.0, y: 0.0 }
                   , friction: 0.9
                   , rotation: (-0.25)
                   , mass: 1000.0
                   , health: maxHealth
                   , shield: maxShield
                   , networkSync: true
                   , behaviour:  Damageable.init 
                               : FiresBullets.init { max: 100, speed: 25.0, rate: 5, power: 25.0 }
                               : BasicBitchPhysics.init 
                               : Driven.init { maxSpeed: 5.0, acceleration: 1500.0, turningSpeed: 0.015 } 
                               : NetworkSync.init { force: 0.05 } 
                               : Regenerates.init { maxHealth, maxShield, healthDelay: 0, healthRegen: 0.0, shieldDelay: 180, shieldRegen: 0.2 }
                               : Nil
                   , renderables : (sprite {transform = { x: (-15.0)
                                                , y: (-15.0)
                                                , width: 30.0
                                                , height: 30.0
                                                }
                                     , image = Just "shield"
                                     , id = "shield"
                                     }) 
                                  : (sprite {transform = { x: (-12.5)
                                                , y: (-12.5)
                                                , width: 25.0
                                                , height: 25.0
                                                }
                                     , image = Just "ship"
                                     , id = "ship"
                                     }) 
                                 : Nil
                                 }
