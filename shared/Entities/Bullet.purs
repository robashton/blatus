module Pure.Entities.Bullet where

import Prelude

import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Pure.Behaviours.BasicBitchPhysics as BasicBitchPhysics
import Pure.Entity (Entity, EntityClass(..), EntityId, HtmlColor(..))
import Pure.Math (Point)


data EntityMode = Server | Client

init :: EntityId -> Point -> Point -> Entity
init id location velocity = { id
             , location: location
             , class: Bullet
             , width: 5.0
             , height: 5.0
             , velocity: velocity
             , friction: 1.0
             , rotation: 0.0
             , mass: 20.0
             , networkSync: false
             , behaviour : BasicBitchPhysics.init : Nil
             , renderables : ({ transform: { x: -2.5
                                           , y: -2.5
                                           , width: 5.0
                                           , height: 5.0
                                           }
                              , rotation: 0.0
                              , color: HtmlColor "#ff0"
                              , image: Nothing
                              }) : Nil
                            }
