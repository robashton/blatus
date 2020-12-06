module Pure.Entities.Tank where

import Prelude

import Control.Apply (lift2)
import Control.Monad.State as State
import Data.Array as Array
import Data.Exists (Exists, mkExists, runExists)
import Data.Foldable (foldl, class Foldable)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.List (List(..), concat, foldr, (:))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Sequence as Seq
import Data.Traversable (find)
import Data.Tuple (Tuple(..), fst, snd)
import Math (cos, pi, pow, sin, sqrt) as Math
import Pure.Behaviour as B
import Pure.Behaviours.BasicBitchPhysics as BasicBitchPhysics
import Pure.Entity (Entity, EntityId(..), EntityCommand(..), GameEvent(..), HtmlColor(..), EntityBehaviour(..), EntityClass(..))
import Pure.Entity as Entity
import Pure.Math (Rect, Point, scalePoint, rotationToVector, lerp)
import Simple.JSON (class ReadForeign, class WriteForeign)

import Pure.Behaviours.BasicBitchPhysics as BasicBitchPhysics
import Pure.Behaviours.HasHealth as HasHealth
import Pure.Behaviours.FiresBullets as FiresBullets
import Pure.Behaviours.Driven as Driven
import Pure.Behaviours.NetworkSync as NetworkSync

data EntityMode = Server | Client

init :: EntityId -> EntityMode -> Point -> Entity
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
