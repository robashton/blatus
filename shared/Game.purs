module Pure.Game where

-- Rough plan :: this is  the shared model, and an instance of it is executed per 'game'
-- On the server that means a gen-server, on the client it means once per player in that game
-- Commands sent to the player entity, are sent to the server and re-written to be sent to the appropriate entity
-- both on server model and other player's client models
-- Events for significant decisions should be raised by all models, events sent by client model can be ignored
-- Events sent from server model to clients (and indeed to itself) are used to enforce the critical decisions
-- That means things like collisions causing health changes (ButtetHitPlayer), and health getting less than 0 (PlayerDestroyed)
-- The server will also send a specific command to each client, containing the actual location of the entities, and we'll do some rubber banding - #sorrynotsorry

import Prelude

import Data.Exists (Exists, mkExists, runExists)
import Data.Foldable (foldl)
import Data.List (List(..), foldr, (:))
import Data.Maybe (Maybe(..))
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
                  , image :: Maybe String
                  , rotation :: Number
                  }

data EntityCommand = Damage Number | Tick | PushForward | PushBackward | TurnLeft | TurnRight

derive instance eqEntityCommand :: Eq EntityCommand

data EntityBehaviourResult state = StateUpdated state 
                                 | StateAndEntityUpdated state Entity
                                 | EntityUpdated Entity

newtype EntityCommandHandler state = EntityCommandHandler (EntityCommand -> state -> Entity -> EntityBehaviourResult state)

data EntityBehaviour state = EntityBehaviour { state :: state 
                                             , handleCommand :: EntityCommandHandler state
                                             }

type Entity = { id :: EntityId
              , location :: Point
              , width :: Number
              , height :: Number
              , mass :: Number
              , velocity :: Point
              , friction :: Number
              , health :: Number
              , rotation :: Number
              , renderables :: List Renderable
              , behaviour :: List (Exists EntityBehaviour) --  Would prefer a typeclass based  solution, I'm pretty sure it can exist
              }


type Game =  { entities  :: List (Entity)
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
  game { entities = map (\e -> if e.id == id then processCommand e command else e) entities }

processCommand :: Entity -> EntityCommand -> Entity
processCommand e command = 
  foldr executeCommand e { behaviour = Nil } e.behaviour
       where
             executeCommand = (\behaviour acc -> runExists (runBehaviour acc) behaviour)
             runBehaviour :: forall state. Entity -> EntityBehaviour state -> Entity
             runBehaviour entity@{ behaviour: behaviourList } (EntityBehaviour behaviour@{ handleCommand: (EntityCommandHandler handler)}) = case handler command behaviour.state entity of
                                                                  StateUpdated newState ->
                                                                    entity { behaviour = ( mkExists $ EntityBehaviour behaviour { state = newState }) : behaviourList }
                                                                  StateAndEntityUpdated newState newEntity ->
                                                                    newEntity { behaviour = ( mkExists $ EntityBehaviour behaviour { state = newState }) : behaviourList }
                                                                  EntityUpdated newEntity ->
                                                                    newEntity { behaviour = ( mkExists $ EntityBehaviour behaviour)  : behaviourList  }

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
                   , health: 100.0
                   , behaviour : basicBitchPhysics : (driven { maxSpeed: 5.0, acceleration: 30.0, turningSpeed: 0.03 } : Nil)  
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


type DrivenConfig = { maxSpeed :: Number
                    , acceleration :: Number
                    , turningSpeed :: Number
                    }

driven :: DrivenConfig -> Exists EntityBehaviour
driven config = mkExists $ EntityBehaviour {  state: unit
                                            , handleCommand: EntityCommandHandler \command _ entity@{ rotation } ->
                                              EntityUpdated $ case command of
                                                                  PushForward -> applyThrust config.acceleration config.maxSpeed entity
                                                                  PushBackward -> applyThrust (-config.acceleration) config.maxSpeed entity
                                                                  TurnLeft -> entity { rotation = rotation - config.turningSpeed }
                                                                  TurnRight -> entity { rotation = rotation + config.turningSpeed }
                                                                  _ -> entity
                                          }

basicBitchPhysics :: Exists EntityBehaviour 
basicBitchPhysics = mkExists $ EntityBehaviour { state: unit
                                               , handleCommand: EntityCommandHandler \command _ e@{ location, velocity, friction, health } ->
                                                                  EntityUpdated $ case command of 
                                                                                        Tick -> e { location = location + velocity, velocity = scalePoint friction velocity }
                                                                                        Damage amount -> 
                                                                                          e { health = health - amount } -- if < 0 then raise that event.. (and that'll only get handled on the server)
                                                                                        _ -> e
                                               }

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
  applyPhysics $ game { entities = map (\e -> processCommand e Tick) entities }


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


