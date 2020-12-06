module Pure.BuiltIn.Collider where

import Prelude

import Control.Apply (lift2)
import Data.Array (foldl)
import Data.Array as Array
import Data.Exists (mkExists)
import Data.List (List(..), (:))
import Data.Map as Map
import Data.Maybe (fromMaybe)
import Math as Math

import Pure.Entity (Entity, EntityBehaviour(..), EntityId(..), EntityCommandHandler, emptyEntity)
import Pure.Entity as Entity
import Pure.Math (Point)
import Pure.Runtime.Scene (TickState)

type CollisionInfo = { left :: EntityId
                     , right :: EntityId
                     , force :: Number 
                     }

onTick :: forall cmd ev. (CollisionInfo -> ev) -> TickState cmd ev -> TickState cmd ev
onTick fn state = 
  foldl (collideEntity fn state.entityCount) state state.entityRange

collideEntity :: forall cmd ev. (CollisionInfo -> ev) -> Int -> TickState cmd ev -> Int -> TickState cmd ev
collideEntity fn termination state index 
  | index == termination = state
  | otherwise = foldl (collidePair fn index) state $ Array.range (index+1) termination

collidePair :: forall cmd ev. (CollisionInfo -> ev) -> Int -> TickState cmd ev -> Int -> TickState cmd ev
collidePair fn li state ri =
  fromMaybe state $ lift2 (\left right ->
    if squareCheck left right then
      let lf = (magnitude left.velocity) * left.mass
          rf = (magnitude right.velocity) * right.mass
          ur = Entity.applyForce { direction: (vectorBetween left.location right.location), force: lf } right -- force from left to right, applied to right
          ul = Entity.applyForce { direction: (vectorBetween right.location left.location), force: rf } left -- force from right to left, applied to left
       in
         state { entities = Map.insert ri ur $ Map.insert li ul state.entities
               , events = (fn { left: left.id, right: right.id, force: lf }
                         : fn { left: right.id, right: left.id, force: rf }
                         : Nil) : state.events 
               }
    else
      state) (Map.lookup li state.entities) (Map.lookup ri state.entities)
        

vectorBetween :: Point -> Point -> Point
vectorBetween s d =
  normalise (d - s)

normalise :: Point -> Point
normalise point@{ x, y } = { x: x / den, y: y / den }
  where den = magnitude point

magnitude :: Point -> Number
magnitude {x, y} = Math.sqrt $ (x * x) + (y * y)

squareCheck :: forall cmd ev. Entity cmd ev -> Entity cmd ev -> Boolean
squareCheck inner subject 
  | inner.location.x > subject.location.x + subject.width = false
  | inner.location.y > subject.location.y + subject.height = false
  | subject.location.x > inner.location.x + inner.width = false
  | subject.location.y > inner.location.y + inner.height = false
  | otherwise = true

circleCheck :: forall cmd ev. Entity cmd ev -> Entity cmd ev -> Boolean
circleCheck inner subject = 
  let distSq = (Math.pow (inner.location.x - subject.location.x) 2.0)
             + (Math.pow (inner.location.y - subject.location.y) 2.0) 
      combinedSizeSq = Math.pow ((max inner.width inner.height) / 2.0 + (max subject.width subject.height) / 2.0) 2.0
   in distSq < combinedSizeSq 





