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
import Pure.Entity (Entity, EntityId(..), EntityCommand(..), GameEvent(..), HtmlColor(..), EntityBehaviour(..), EntityClass(..))
import Pure.Entity as Entity
import Pure.Math (Rect, Point, scalePoint, rotationToVector, lerp)
import Simple.JSON (class ReadForeign, class WriteForeign)

type EntityMap = Map EntityId Entity

type Game =  { entities  :: EntityMap
             , world :: Rect
  }

initialModel :: Game
initialModel = { entities : mempty
               , world:  { x: -1000.0, y: -1000.0, width: 2000.0, height: 2000.0 }
}


sendCommand :: EntityId -> EntityCommand -> Game -> Tuple Game (List GameEvent)
sendCommand id command game@{ entities } =
  case (Map.lookup id entities) of
    Nothing -> Tuple game Nil
    Just entity -> let (Tuple newEntity evs) = Entity.processCommand entity command 
                   in Tuple (game { entities = Map.insert id newEntity entities }) evs

foldEvents :: forall m. Foldable m => Game -> (m GameEvent) -> Game
foldEvents game events = foldr sendEvent game events

discardEvents :: Tuple Game (List GameEvent) -> Game
discardEvents (Tuple game _events) = game

sendEvent :: GameEvent -> Game -> Game
sendEvent ev game = 
  case ev of
       BulletFired deets -> addEntity (bullet deets.id deets.location deets.velocity) game
       EntityCollided _ -> game

addEntity :: Entity -> Game -> Game
addEntity entity game =
  game { entities = Map.insert entity.id entity game.entities }

updateEntity :: (Entity -> Entity) -> EntityId -> Game -> Game
updateEntity fn id game =
  game { entities = Map.update (fn >>> Just) id game.entities }

removeEntity :: EntityId -> Game -> Game
removeEntity id game =
  game { entities = Map.delete id game.entities }


entityById :: EntityId -> Game -> Maybe Entity
entityById id { entities } =
  find (\e -> e.id == id) entities
  
data EntityMode = Server | Client

tank :: EntityId -> EntityMode -> Point -> Entity
tank id mode location = { id
                        , location
                        , class: Tank
                        , width: 25.0
                        , height: 25.0
                        , velocity: { x: 0.0, y: 0.0 }
                        , friction: 0.9
                        , rotation: (-0.25)
                        , mass: 1000.0
                        , networkSync: true
                        , behaviour:  hasHealth 100.0 
                                    : firesBullets { max: 100, speed: 15.0, rate: 5 }
                                    : basicBitchPhysics 
                                    : driven { maxSpeed: 5.0, acceleration: 1500.0, turningSpeed: 0.03 } 
                                    : case mode of 
                                        Server -> Nil
                                        Client -> (networkSync { force: 0.05 }) : Nil
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
             , class: Bullet
             , width: 5.0
             , height: 5.0
             , velocity: velocity
             , friction: 1.0
             , rotation: 0.0
             , mass: 20.0
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

type ElasticConfig = { force :: Number }

networkSync :: ElasticConfig -> Exists EntityBehaviour
networkSync c = mkExists $ EntityBehaviour { state: { force: c.force
                                                    , location: { x: 0.0, y: 0.0 }
                                                    , rotation: 0.0
                                                    , velocity: { x: 0.0, y: 0.0 }
                                                    , oldLocation: { x: 0.0, y: 0.0 }
                                                    , oldRotation: 0.0
                                                    }
                                           , handleCommand: handleCommand
                                           }
                 where handleCommand command s = do
                         e <- B.entity 
                         case command of
                           UpdateServerState ss ->
                             pure $ s { oldLocation = e.location
                                      , oldRotation = e.rotation
                                      , location = ss.location
                                      , rotation = ss.rotation
                                      }
                           Tick -> do 
                             let targetRotation = s.rotation + (e.rotation - s.oldRotation)
                                 targetLocation = s.location + (e.location - s.oldLocation)
                                 newLocation = lerp e.location targetLocation s.force
                                 newRotation = e.rotation + s.force * (targetRotation - e.rotation) 
                             _ <- B.updateEntity (\entity -> entity { location = newLocation
                                                                    , rotation = newRotation
                                                                    })
                             pure s { rotation = targetRotation
                                    , location = targetLocation
                                    , oldRotation = newRotation
                                    , oldLocation = newLocation
                                    }
                           _ -> pure s



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


type DrivenConfig = { maxSpeed :: Number
                    , acceleration :: Number
                    , turningSpeed :: Number
                    }
                                                                    
driven :: DrivenConfig -> Exists EntityBehaviour
driven config = mkExists $ EntityBehaviour {  state: { forward: false, backward: false, left: false, right: false }
                                            , handleCommand:  \command s  ->
                                                case command of
                                                  Tick -> do
                                                    (if s.forward then B.applyThrust config.acceleration config.maxSpeed
                                                          else if s.backward then B.applyThrust (-config.acceleration) config.maxSpeed
                                                          else pure unit)
                                                    (if s.left then B.rotate (-config.turningSpeed)
                                                          else if s.right then B.rotate config.turningSpeed 
                                                          else pure unit)
                                                    pure s
                                                  PushForward -> pure s { forward = true }
                                                  PushBackward -> pure s { backward = true }
                                                  TurnLeft -> pure s { left = true }
                                                  TurnRight -> pure s { right = true }
                                                  StopPushForward -> pure s { forward = false }
                                                  StopPushBackward -> pure s { backward = false }
                                                  StopTurnLeft -> pure s { left = false }
                                                  StopTurnRight -> pure s { right = false }
                                                  _ -> 
                                                    pure s
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
                                             

type TickState = { entities :: Map.Map Int Entity
                 , events :: List (List GameEvent)
                 }
-- entities = Map.fromFoldable final.entities  

tick :: Game -> Tuple Game (List GameEvent)
tick game@ { entities } =
  Tuple (game { entities = foldl (\m e -> Map.insert e.id e m) mempty (Map.values final.entities) }) $ concat final.events
  where
        final = foldl (collideEntity length)  tickedState range
        tickedState = foldl tickEntity initialState range
        range = Array.range 0 length
        length = Map.size initialState.entities
        initialState = { entities: foldlWithIndex (\i m (Tuple k v) -> Map.insert i v m) mempty $ (Map.toUnfoldableUnordered entities :: List (Tuple EntityId Entity))
                       , events: Nil
                       }

tickEntity :: TickState -> Int -> TickState 
tickEntity state index = 
  maybe state (\e -> let
                        (Tuple newEntity newEvents) = Entity.processCommand e Tick 
                      in
                        state { events = newEvents : state.events
                              , entities = Map.insert index newEntity state.entities
                              }
              ) $ Map.lookup index state.entities
--  

collideEntity :: Int -> TickState -> Int -> TickState
collideEntity termination state index 
  | index == termination = state
  | otherwise = foldl (collidePair index) state $ Array.range (index+1) termination

collidePair :: Int -> TickState -> Int -> TickState
collidePair li state ri =
  fromMaybe state $ lift2 (\left right ->
    if squareCheck left right then
      let lf = (magnitude left.velocity) * left.mass
          rf = (magnitude right.velocity) * right.mass
          ur = Entity.applyForce { direction: (vectorBetween left.location right.location), force: lf } right -- force from left to right, applied to right
          ul = Entity.applyForce { direction: (vectorBetween right.location left.location), force: rf } left -- force from right to left, applied to left
       in
         state { entities = Map.insert ri ur $ Map.insert li ul state.entities
               , events = (EntityCollided { one: left.id, two: right.id, force: lf }
                         : EntityCollided { one: right.id, two: left.id, force: rf }
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

squareCheck :: Entity -> Entity -> Boolean
squareCheck inner subject 
  | inner.location.x > subject.location.x + subject.width = false
  | inner.location.y > subject.location.y + subject.height = false
  | subject.location.x > inner.location.x + inner.width = false
  | subject.location.y > inner.location.y + inner.height = false
  | otherwise = true

circleCheck :: Entity -> Entity -> Boolean
circleCheck inner subject = 
  let distSq = (Math.pow (inner.location.x - subject.location.x) 2.0)
             + (Math.pow (inner.location.y - subject.location.y) 2.0) 
      combinedSizeSq = Math.pow ((max inner.width inner.height) / 2.0 + (max subject.width subject.height) / 2.0) 2.0
   in distSq < combinedSizeSq 


