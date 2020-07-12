module Pure.Game where

-- Rough plan :: this is  the shared model, and an instance of it is executed per 'game'
-- On the server that means a gen-server, on the client it means once per player in that game
-- Commands sent to the player entity, are sent to the server and re-written to be sent to the appropriate entity
-- both on server model and other player's client models
-- Events for significant decisions should be raised by all models, events sent by client model can be ignored
-- Events sent from server model to clients (and indeed to itself) are used to enforce the critical decisions
-- That means things like collisions causing health changes (ButtetHitPlayer), and health getting less than 0 (PlayerDestroyed)
-- The server will also send a specific command to each client, containing the actual location of the entities, and we'll do some rubber banding - #sorrynotsorry
-- For now, we'll allow the events to be client-side, because there is no server, but that'll be the working practise

import Prelude

import Data.Exists (Exists, mkExists, runExists)
import Data.Foldable (foldl)
import Data.List (List(..), concat, foldr, (:))
import Data.Map (Map)
import Data.Map (fromFoldable, insert, lookup, mapMaybe, values) as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Traversable (find)
import Data.Tuple (Tuple(..), fst, snd)
import Math (cos, pi, pow, sin, sqrt) as Math
import Pure.Math (Rect, Point, scalePoint, rotationToVector)
import Simple.JSON (class ReadForeign, class WriteForeign)
import Pure.Entity (Entity, EntityId(..), EntityCommand(..), GameEvent(..), HtmlColor(..), EntityBehaviour(..))
import Pure.Entity as Entity
import Pure.Behaviour as B 


type EntityMap = Map EntityId Entity

type Game =  { entities  :: EntityMap
             , world :: Rect
  }

initialModel :: Game
initialModel = { entities : Map.fromFoldable $ (Tuple (EntityId "player") (tank (EntityId "player") { x: 20.0, y: 20.0 } )) :
                                               (Tuple (EntityId "jimmy") (tank (EntityId "jimmy") { x: 300.0, y: 500.0 } )) :
                                               (Tuple (EntityId "stacko") (tank (EntityId "stacko") { x: 200.0, y: 100.0 })) :
                                               (Tuple (EntityId "daniel") (tank (EntityId "daniel") { x: -200.0, y: -300.0 })) : Nil
               , world:  { x: -1000.0, y: -1000.0, width: 2000.0, height: 2000.0 }
}


sendCommand :: EntityId -> EntityCommand -> Game -> Tuple Game (List GameEvent)
sendCommand id command game@{ entities } =
  case (Map.lookup id entities) of
    Nothing -> Tuple game Nil
    Just entity -> let (Tuple newEntity evs) = Entity.processCommand entity command 
                   in Tuple (game { entities = Map.insert id newEntity entities }) evs

foldEvents :: Tuple Game (List GameEvent) -> Game
foldEvents (Tuple game events) = foldr sendEvent game events

sendEvent :: GameEvent -> Game -> Game
sendEvent ev game = 
  case ev of
       BulletFired deets -> addEntity (bullet deets.id deets.location deets.velocity) game

addEntity :: Entity -> Game -> Game
addEntity entity game =
  game { entities = Map.insert entity.id entity game.entities }


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
                   , networkSync: true
                   , behaviour : hasHealth 100.0 
                   : firesBullets { max: 100, speed: 15.0, rate: 5 }
                               : basicBitchPhysics 
                               : (driven { maxSpeed: 5.0, acceleration: 15.0, turningSpeed: 0.03 } 
                               : Nil)  

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

bullet :: EntityId -> Point -> Point -> Entity
bullet id location velocity = { id
             , location: location
             , width: 5.0
             , height: 5.0
             , velocity: velocity
             , friction: 1.0
             , rotation: 0.0
             , mass: 200.0
             , networkSync: false
             , behaviour : basicBitchPhysics : Nil
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


type DrivenConfig = { maxSpeed :: Number
                    , acceleration :: Number
                    , turningSpeed :: Number
                    }

firesBullets :: { max :: Int, speed :: Number, rate:: Int } -> Exists EntityBehaviour
firesBullets { max, speed, rate } = mkExists $ EntityBehaviour { state: { current: 0, firingTimer: 0 }
                                                               , handleCommand: handleCommand
                                                         }
             where handleCommand command state@{ current, firingTimer } = do
                                             entity <- B.entity 
                                             case command of
                                               FireBullet -> 
                                                 if firingTimer <= 0 then do
                                                   B.raiseEvent $ (BulletFired { id: id entity, location: location entity, velocity: velocity entity }) 
                                                   pure state { current = current + 1, firingTimer = rate }
                                                 else
                                                   pure state
                                               Tick -> pure $ state { current = if current > max then 0 else current, 
                                                                firingTimer = if firingTimer > 0 then firingTimer - 1 else firingTimer
                                                               }

                                               _ -> pure state
                                          where id entity = wrap $ (unwrap entity.id) <> "-bullet-" <> (show state.current)
                                                direction entity = rotationToVector entity.rotation
                                                location entity = entity.location + (scalePoint entity.width $ direction entity)
                                                velocity entity = (scalePoint speed $ direction entity) + entity.velocity

                                                                    
                                                    

driven :: DrivenConfig -> Exists EntityBehaviour
driven config = mkExists $ EntityBehaviour {  state: unit
                                            , handleCommand:  \command s  ->
                                                case command of
                                                  PushForward -> B.applyThrust config.acceleration config.maxSpeed
                                                  PushBackward -> B.applyThrust (-config.acceleration) config.maxSpeed
                                                  TurnLeft -> B.rotate (-config.turningSpeed)
                                                  TurnRight -> B.rotate config.turningSpeed 
                                                  _ -> pure unit
                                                  
                                          }

basicBitchPhysics :: Exists EntityBehaviour 
basicBitchPhysics = mkExists $ EntityBehaviour { state: unit
                                               , handleCommand:  \command _ ->
                                                 case command of 
                                                      Tick -> do
                                                        B.updateEntity (\e@{ location, velocity, friction }  -> 
                                                          e { location = location + velocity, velocity = scalePoint friction velocity })
                                                      _ -> pure unit
                                               }

hasHealth :: Number -> Exists EntityBehaviour 
hasHealth amount = mkExists $ EntityBehaviour { state: amount
                                               , handleCommand:  \command state ->
                                                                  case command of 
                                                                       Damage damage -> pure $ state - damage
                                                                       _ -> pure state
                                               }
                                             


tick :: Game -> Tuple Game (List GameEvent)
tick game@ { entities } =
   let result = foldl tickEntity { entities: mempty, events: Nil } entities
    in Tuple (applyPhysics $ game { entities = result.entities }) result.events
  where
        tickEntity ::  { entities :: EntityMap, events :: List GameEvent } -> Entity -> { entities :: EntityMap, events :: List GameEvent }
        tickEntity acc entity = let (Tuple newEntity newEvents) =  Entity.processCommand entity Tick 
                                                  in { entities:  Map.insert newEntity.id newEntity acc.entities, events: concat $ acc.events : newEvents : Nil   }



-- Note: There isn't a fn for it in core PS, but we effectively 
-- need an efficient crossjoin-map, as there is little sense
-- in doing an n^2 operation here
applyPhysics :: Game -> Game
applyPhysics game@{ entities } = 
  game { entities = Map.mapMaybe (performChecks (Map.values entities)) entities }

performChecks :: List Entity -> Entity -> Maybe Entity
performChecks entities target =
  Just $ foldl collideEntities target entities

collideEntities :: Entity -> Entity -> Entity
collideEntities target e 
  | target.id == e.id = target
  | otherwise =
      if circleCheck target e then 
        Entity.applyForce { direction: (vectorBetween e.location target.location)
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


circleCheck :: Entity -> Entity -> Boolean
circleCheck inner subject = 
  let distSq = (Math.pow (inner.location.x - subject.location.x) 2.0)
             + (Math.pow (inner.location.y - subject.location.y) 2.0) 
      combinedSizeSq = Math.pow ((max inner.width inner.height) / 2.0 + (max subject.width subject.height) / 2.0) 2.0
   in distSq < combinedSizeSq 


