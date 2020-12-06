module Pure.Entity where

import Prelude
import Control.Monad.State (State, runState)
import Data.Exists (Exists, mkExists, runExists)
import Data.List (List(..), concat, foldr, (:))
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple(..), fst, snd)
import Pure.Math (Rect, Point, scalePoint)
import Simple.JSON (class ReadForeign, class WriteForeign)
import GenericJSON (writeTaggedSumRep, taggedSumRep)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)

newtype HtmlColor = HtmlColor String
derive instance ntHtmlColor :: Newtype HtmlColor _

newtype EntityId = EntityId String
derive instance ntEntityId :: Newtype EntityId _
derive newtype instance eqEntityId :: Eq EntityId
derive newtype instance readEntityId :: ReadForeign EntityId
derive newtype instance writeEntityId :: WriteForeign EntityId
derive newtype instance showEntityId :: Show EntityId
derive newtype instance ordEntityId :: Ord EntityId

-- Need this to disappear at some point
-- It can probably just be an arbitrary string as it's just for serialization purposes
data EntityClass = Tank | Bullet

derive instance genericEntityClass :: Generic EntityClass _
instance showEntityClass :: Show EntityClass where
  show = genericShow
instance writeForeignEntityClass :: WriteForeign EntityClass where
  writeImpl = writeTaggedSumRep
instance readForeignEntityClass :: ReadForeign EntityClass where
  readImpl = taggedSumRep

type Renderable = { transform :: Rect
                  , color :: HtmlColor
                  , image :: Maybe String
                  , rotation :: Number
                  }

data EntityBehaviourResult cmd ev state  = StateUpdated state 
                                         | StateAndEntityUpdated state (Entity cmd ev)
                                         | EntityUpdated (Entity cmd ev)
                                         | RaiseEvent ev state
                                         | NoOp


type BehaviourExecutionContext cmd ev = { events :: List ev
                                    , entity :: (Entity cmd ev)
                                    }

type EntityCommandHandlerResult cmd ev state =  State (BehaviourExecutionContext cmd ev) state
type EntityCommandHandler cmd ev state = cmd -> state -> (EntityCommandHandlerResult cmd ev state) 

data EntityBehaviour cmd ev state = EntityBehaviour { state :: state 
                                                    , handleCommand :: EntityCommandHandler cmd ev state
                                                    }

type Entity cmd ev = { id :: EntityId
                     , class :: EntityClass
                     , location :: Point
                     , width :: Number
                     , height :: Number
                     , mass :: Number
                     , velocity :: Point
                     , friction :: Number
                     , rotation :: Number
                     , renderables :: List Renderable
                     , networkSync :: Boolean
                     , behaviour :: List (Exists (EntityBehaviour cmd ev))
                     }

emptyEntity :: forall cmd ev. EntityId -> Entity cmd ev
emptyEntity id = { id
                 , class: Bullet
                 , location: { x: 0.0, y: 0.0 }
                 , width: 0.0
                 , height: 0.0
                 , mass: 0.0
                 , velocity : { x: 0.0, y: 0.0 }
                 , friction: 0.0
                 , rotation: 0.0
                 , renderables: Nil
                 , networkSync: false
                 , behaviour: Nil
                 }
            

processCommand :: forall cmd ev. (Entity cmd ev) -> cmd -> Tuple (Entity cmd ev) (List ev)
processCommand e command = 
  foldr executeCommand (Tuple (e { behaviour = Nil }) Nil) e.behaviour
       where
             executeCommand :: Exists (EntityBehaviour cmd ev) -> (Tuple (Entity cmd ev) (List ev)) -> (Tuple (Entity cmd ev) (List ev)) 
             executeCommand = (\behaviour (Tuple acc evs) -> let runResult = runExists (runBehaviour acc) behaviour
                                                             in Tuple (fst runResult) (concat $ (snd runResult) : evs : Nil))
             runBehaviour :: forall state. Entity cmd ev -> EntityBehaviour cmd ev state -> Tuple (Entity cmd ev) (List ev)
             runBehaviour entity@{ behaviour: behaviourList } (EntityBehaviour behaviour@{ handleCommand: (handler)}) = 
               let 
                   Tuple newState result = runState (handler command behaviour.state) { entity, events: Nil  }
                   newEntity = result.entity
                   newEvents = result.events
                in
                  Tuple (newEntity { behaviour = ( mkExists $ EntityBehaviour behaviour { state = newState }) : behaviourList }) newEvents

applyForce :: forall cmd ev. { direction :: Point, force :: Number } -> Entity cmd ev -> Entity cmd ev
applyForce { direction, force } entity@{ velocity, mass } = 
  entity { velocity = velocity + (scalePoint (force / mass) direction) }
