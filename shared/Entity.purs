module Pure.Entity where

import Prelude
import Control.Monad.State (State)
import Control.Monad.State (lift) as Exports
import Control.Monad.State (runState, execState, evalState, lift)
import Control.Monad.State as State
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

data EntityCommand = Damage Number 
                   | Tick 
                   | PushForward 
                   | PushBackward 
                   | TurnLeft 
                   | TurnRight
                   | FireBullet

data GameEvent = BulletFired { id :: EntityId, location :: Point, velocity :: Point }

derive instance eqEntityCommand :: Eq EntityCommand

data EntityBehaviourResult state = StateUpdated state 
                                 | StateAndEntityUpdated state Entity
                                 | EntityUpdated Entity
                                 | RaiseEvent GameEvent state
                                 | NoOp


type BehaviourExecutionContext = { events :: List GameEvent
                                 , entity :: Entity
                                 }

type EntityCommandHandlerResult state =  State BehaviourExecutionContext state
type EntityCommandHandler state = EntityCommand -> state -> EntityCommandHandlerResult state 

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
              , rotation :: Number
              , renderables :: List Renderable
              , networkSync :: Boolean
              , behaviour :: List (Exists EntityBehaviour)
              }

processCommand :: Entity -> EntityCommand -> Tuple Entity (List GameEvent)
processCommand e command = 
  foldr executeCommand (Tuple (e { behaviour = Nil }) Nil) e.behaviour
       where
             executeCommand :: Exists EntityBehaviour -> (Tuple Entity (List GameEvent)) -> (Tuple Entity (List GameEvent)) 
             executeCommand = (\behaviour (Tuple acc evs) -> let runResult = runExists (runBehaviour acc) behaviour
                                                             in Tuple (fst runResult) (concat $ (snd runResult) : evs : Nil))
             runBehaviour :: forall state. Entity -> EntityBehaviour state -> Tuple Entity (List GameEvent)
             runBehaviour entity@{ behaviour: behaviourList } (EntityBehaviour behaviour@{ handleCommand: (handler)}) = 
               let 
                   Tuple newState result = runState (handler command behaviour.state) { entity, events: Nil  }
                   newEntity = result.entity
                   newEvents = result.events
                in
                  Tuple (newEntity { behaviour = ( mkExists $ EntityBehaviour behaviour { state = newState }) : behaviourList }) newEvents

applyForce :: { direction :: Point, force :: Number } -> Entity -> Entity
applyForce { direction, force } entity@{ velocity, mass } = 
  entity { velocity = velocity + (scalePoint (force / mass) direction) }

