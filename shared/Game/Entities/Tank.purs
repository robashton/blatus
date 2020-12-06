module Pure.Entities.Tank where

import Prelude

import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Pure.Behaviours.BasicBitchPhysics as BasicBitchPhysics
import Pure.Entity (Entity, EntityClass(..), EntityId, HtmlColor(..))
import Pure.Math (Point)

import Pure.Behaviours.HasHealth as HasHealth
import Pure.Behaviours.FiresBullets as FiresBullets
import Pure.Behaviours.Driven as Driven
import Pure.Behaviours.NetworkSync as NetworkSync

import Pure.Types (EntityCommand, GameEvent)

data EntityMode = Server | Client

init :: EntityId -> EntityMode -> Point -> Entity EntityCommand GameEvent
init id mode location = { id
                        , location
                        , class: Tank
                        , width: 25.0
                        , height: 25.0
                        , velocity: { x: 0.0, y: 0.0 }
                        , friction: 0.9
                        , rotation: (-0.25)
                        , mass: 1000.0
                        , networkSync: true
                        , behaviour:  HasHealth.init 100.0 
                                    : FiresBullets.init { max: 100, speed: 15.0, rate: 5 }
                                    : BasicBitchPhysics.init 
                                    : Driven.init { maxSpeed: 5.0, acceleration: 1500.0, turningSpeed: 0.03 } 
                                    : case mode of 
                                        Server -> Nil
                                        Client -> (NetworkSync.init { force: 0.05 }) : Nil
                        , renderables : ({transform: { x: (-12.5)
                                                     , y: (-12.5)
                                                     , width: 25.0
                                                     , height: 25.0
                                                     }
                                        , rotation: 0.0
                                        , color: HtmlColor "#f00"
                                        , image: Just "ship"
                                        }) : Nil
                                      }
