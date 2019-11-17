module Pure.Game where


import Prelude

import Data.Foldable (foldl)
import Data.List (List(..), (:))
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Traversable (find)
import Math (cos, pi, pow, sin, sqrt) as Math
import Pure.Math (Rect, Point, scalePoint)
import Simple.JSON (class ReadForeign, class WriteForeign)

newtype HtmlColor = HtmlColor String
derive instance ntHtmlColor :: Newtype HtmlColor _

newtype EntityId = EntityId String
derive instance ntEntityId :: Newtype EntityId _
derive newtype instance eqEntityId :: Eq EntityId
derive newtype instance readEntityId :: ReadForeign EntityId
derive newtype instance writeEntityId :: WriteForeign EntityId
derive newtype instance showEntityId :: Show EntityId
derive newtype instance ordEntityId :: Ord EntityId

type Renderable = { transform :: Rect
                  , color :: HtmlColor
                  , rotation :: Number
                  }

data EntityCommand = PushForward | PushBackward | TurnLeft | TurnRight

derive instance eqEntityCommand :: Eq EntityCommand

newtype EntityBehaviour = EntityBehaviour (Entity -> Entity)
newtype EntityCommandHandler = EntityCommandHandler (EntityCommand -> Entity -> Entity)

type Entity = { id :: EntityId
              , location :: Point
              , width :: Number
              , height :: Number
              , mass :: Number
              , velocity :: Point
              , friction :: Number
              , rotation :: Number
              , renderables :: List Renderable
              , commandHandlers :: List EntityCommandHandler
              , behaviour :: List EntityBehaviour
              }


type Game =  { entities  :: List Entity 
             , world :: Rect
  }

initialModel :: Game
initialModel = { entities : (tank (EntityId "player") { x: 20.0, y: 20.0 } ) :
                            (tank (EntityId "jimmy") { x: 300.0, y: 500.0 }) :
                            (tank (EntityId "stacko") { x: 200.0, y: 100.0 }) :
                            (tank (EntityId "daniel") { x: -200.0, y: -300.0 }) : Nil
               , world:  { x: -1000.0, y: -1000.0, width: 2000.0, height: 2000.0 }
}


sendCommand :: EntityId -> EntityCommand -> Game -> Game
sendCommand id command game@{ entities } =
  game { entities = map (\e -> if e.id == id then foldl (\acc (EntityCommandHandler h) -> h command acc) e e.commandHandlers
                                 else e) entities
       }

entityById :: EntityId -> Game -> Maybe Entity
entityById id { entities } =
  find (\e -> e.id == id) entities
  

tank :: EntityId -> Point -> Entity
tank id location = { id
                   , location
                   , width: 25.0
                   , height: 25.0
                   , velocity: { x: 0.0, y: 0.0 }
                   , friction: 0.9
                   , rotation: (-0.25)
                   , mass: 10.0
                   , behaviour : basicBitchPhysics : Nil
                   , commandHandlers : (driven { maxSpeed: 5.0, acceleration: 30.0, turningSpeed: 0.03 } : Nil)  
                   , renderables : ({transform: { x: (-12.5)
                                                , y: (-12.5)
                                                , width: 25.0
                                                , height: 25.0
                                                }
                                   , rotation: 0.0
                                   , color: HtmlColor "#f00"
                                   }) : Nil
                                 }


type DrivenConfig = { maxSpeed :: Number
                    , acceleration :: Number
                    , turningSpeed :: Number
                    }

driven :: DrivenConfig -> EntityCommandHandler
driven config = EntityCommandHandler \command entity@{ rotation } ->
  case command of
       PushForward -> applyThrust config.acceleration config.maxSpeed entity
       PushBackward -> applyThrust (-config.acceleration) config.maxSpeed entity
       TurnLeft -> entity { rotation = rotation - config.turningSpeed }
       TurnRight -> entity { rotation = rotation + config.turningSpeed }

-- This is more likely to be handled in its own global manner
-- As we'll need do collision detection at the same time if we're to be remotely efficient
-- iirc you can't just do overlaping quads for collision detection either because otherwise they'll
-- just pass through each other (it may be fine keeping basic bitch physics, so long as the collision is pre-emptive)
basicBitchPhysics :: EntityBehaviour 
basicBitchPhysics = EntityBehaviour \e@{ location, velocity, friction } ->
  e { location = location + velocity, velocity = scalePoint friction velocity }

applyThrust :: Number -> Number -> Entity -> Entity
applyThrust accel maxSpeed entity =
  applyForce { direction: { x: xvel, y: yvel }, force: accel } entity
      where 
        angle = entity.rotation  * Math.pi * 2.0
        xvel = (Math.cos angle) 
        yvel = (Math.sin angle)


rotate :: Entity -> Entity
rotate e@{ rotation } =
  e { rotation = rotation + 0.01 }


tick :: Game -> Game 
tick game@ { entities } =
  applyPhysics $ game { entities = map (\e -> foldl (\i (EntityBehaviour f) -> f i) e e.behaviour) entities }


-- Note: There isn't a fn for it in core PS, but we effectively 
-- need an efficient crossjoin-map, as there is little sense
-- in doing an n^2 operation here
applyPhysics :: Game -> Game
applyPhysics game@{ entities } = 
  game { entities = map (performChecks entities) entities }

performChecks :: List Entity -> Entity -> Entity
performChecks entities target =
  foldl collideEntities target entities

collideEntities :: Entity -> Entity -> Entity
collideEntities target e 
  | target.id == e.id = target
  | otherwise =
      if circleCheck target e then 
        applyForce { direction: (vectorBetween e.location target.location)
                   , force : (magnitude e.velocity) * e.mass
                   }  target
       else
       target

vectorBetween :: Point -> Point -> Point
vectorBetween s d =
  normalise (d - s)

normalise :: Point -> Point
normalise point@{ x, y } = { x: x / den, y: y / den }
  where den = magnitude point

magnitude :: Point -> Number
magnitude {x, y} = Math.sqrt $ (x * x) + (y * y)

scale :: Number -> Point -> Point
scale s { x ,y } = { x: x * s, y: y * s } 

applyForce :: { direction :: Point, force :: Number } -> Entity -> Entity
applyForce { direction, force } entity@{ velocity, mass } = 
  entity { velocity = velocity + (scale (force / mass) direction) }

circleCheck :: Entity -> Entity -> Boolean
circleCheck inner subject = 
  let distSq = (Math.pow (inner.location.x - subject.location.x) 2.0)
             + (Math.pow (inner.location.y - subject.location.y) 2.0) 
      combinedSizeSq = Math.pow ((max inner.width inner.height) / 2.0 + (max subject.width subject.height) / 2.0) 2.0
   in distSq < combinedSizeSq 































