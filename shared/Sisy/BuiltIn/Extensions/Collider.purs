module Sisy.BuiltIn.Extensions.Collider where

import Prelude
import Control.Apply (lift2)
import Data.Array (foldl)
import Data.Array as Array
import Data.List (List(..), (:))
import Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Data.Variant (Variant, inj)
import Debug (spy)
import Math as Math
import Sisy.BuiltIn.Behaviours.BasicBitchPhysics (Mass(..))
import Sisy.BuiltIn.Behaviours.BasicBitchPhysics as BasicBitchPhysics
import Sisy.Math (Point, scalePoint)
import Sisy.Runtime.Entity (Entity, EntityId)
import Sisy.Runtime.Scene (TickState)
import Unsafe.Coerce (unsafeCoerce)

type CollisionInfo
  = { left :: EntityId
    , right :: EntityId
    , force :: Number
    }

type Event r
  = ( entityCollided :: CollisionInfo
    | r
    )

entityCollided :: forall r. CollisionInfo -> Variant (Event r)
entityCollided = inj (SProxy :: SProxy "entityCollided")

onTick ::
  forall cmd ev entity. TickState cmd (Event ev) (BasicBitchPhysics.Required entity) -> TickState cmd (Event ev) (BasicBitchPhysics.Required entity)
onTick state = foldl (collideEntity state.entityCount) state state.entityRange

collideEntity ::
  forall cmd ev entity. Int -> TickState cmd (Event ev) (BasicBitchPhysics.Required entity) -> Int -> TickState cmd (Event ev) (BasicBitchPhysics.Required entity)
collideEntity termination state index
  | index == termination = state
  | otherwise = foldl (collidePair index) state $ Array.range (index + 1) termination

collidePair ::
  forall cmd ev entity. Int -> TickState cmd (Event ev) (BasicBitchPhysics.Required entity) -> Int -> TickState cmd (Event ev) (BasicBitchPhysics.Required entity)
collidePair li state ri =
  fromMaybe state
    $ lift2
        ( \left right ->
            -- handwavey as **** but as long as it's fun I don't care
            if squareCheck left right then
              let
                applyForce target source = case target.mass of
                  Infinite -> Tuple 0.0 target -- we're immovable, shrug it off
                  Fixed targetMass -> case source.mass of
                    Infinite ->  -- we've hit an immovable object so we just need to do the standard 'get the hell off it' thing
                      let
                        force = (magnitude target.velocity) * targetMass

                        newLocation = target.location - target.velocity

                        newVelocity = target.velocity * { x: -0.5, y: -0.5 }

                        newTarget = target { location = newLocation, velocity = newVelocity }
                      in
                        Tuple force newTarget
                    Fixed sourceMass -> Tuple 0.0 target

                Tuple lf ul = applyForce left right

                Tuple rf ur = applyForce right left
              --                ul = case left.mass of
              --                  Fixed leftMass -> 
              --                    let borrowedForce = case right.mass of
              --                                          Infinite -> 0.0
              --                                          Fixed rightMass -> 
              --
              --
              --                                    
              --
              --                    (magnitude left.velocity) * mass
              --                  Infinite -> left
              --
              --                -- And then right
              --                ur = case right.mass of
              --                  Fixed mass -> (magnitude right.velocity) * mass
              --                  Infinite -> 0.0
              --
              --                ur = case right.mass of
              --                  Fixed _ -> BasicBitchPhysics.applyForce { direction: (vectorBetween left.location right.location), force: lf } right
              --                  Infinite -> right
              --
              --                ul = case left.mass of
              --                  Fixed _ -> BasicBitchPhysics.applyForce { direction: (vectorBetween right.location left.location), force: rf } left
              --                  Infinite -> left
              in
                state
                  { entities = Map.insert ri ur $ Map.insert li ul state.entities
                  , events =
                    ( (entityCollided { left: left.id, right: right.id, force: lf })
                        : (entityCollided { left: right.id, right: left.id, force: rf })
                        : Nil
                    )
                      : state.events
                  }
            else
              state
        )
        (Map.lookup li state.entities)
        (Map.lookup ri state.entities)

vectorBetween :: Point -> Point -> Point
vectorBetween s d = normalise (d - s)

normalise :: Point -> Point
normalise point@{ x, y } = { x: x / den, y: y / den }
  where
  den = magnitude point

magnitude :: Point -> Number
magnitude { x, y } = Math.sqrt $ (x * x) + (y * y)

squareCheck :: forall cmd ev entity. Entity cmd ev entity -> Entity cmd ev entity -> Boolean
squareCheck inner subject
  | inner.location.x > subject.location.x + subject.width = false
  | inner.location.y > subject.location.y + subject.height = false
  | subject.location.x > inner.location.x + inner.width = false
  | subject.location.y > inner.location.y + inner.height = false
  | otherwise = true

circleCheck :: forall cmd ev entity. Entity cmd ev entity -> Entity cmd ev entity -> Boolean
circleCheck inner subject =
  let
    distSq =
      (Math.pow (inner.location.x - subject.location.x) 2.0)
        + (Math.pow (inner.location.y - subject.location.y) 2.0)

    combinedSizeSq = Math.pow ((max inner.width inner.height) / 2.0 + (max subject.width subject.height) / 2.0) 2.0
  in
    distSq < combinedSizeSq
