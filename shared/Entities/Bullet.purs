module Pure.Entities.Bullet where

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
