module Sisy.Runtime.Entity where

import Prelude
import Control.Monad.State (State, runState)
import Data.Exists (Exists, mkExists, runExists)
import Data.List (List(..), concat, foldr, (:))
import Data.Maybe (Maybe(..))
import Data.Monoid (class Semigroup)
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Variant (Variant)
import Record as Record
import Simple.JSON (class ReadForeign, class WriteForeign)
import Sisy.Math (Point, Rect)

newtype HtmlColor
  = HtmlColor String

derive instance ntHtmlColor :: Newtype HtmlColor _

newtype EntityId
  = EntityId String

instance semiGroupEntityId :: Semigroup EntityId where
  append (EntityId l) (EntityId r) = EntityId $ l <> r

derive instance ntEntityId :: Newtype EntityId _

derive newtype instance eqEntityId :: Eq EntityId

derive newtype instance readEntityId :: ReadForeign EntityId

derive newtype instance writeEntityId :: WriteForeign EntityId

derive newtype instance showEntityId :: Show EntityId

derive newtype instance ordEntityId :: Ord EntityId

type Renderable
  = { transform :: Rect
    , color :: HtmlColor
    , image :: Maybe String
    , rotation :: Number
    , visible :: Boolean
    , id :: String
    , shape :: Shape
    }

data Shape
  = Circle
  | Rect

sprite :: Renderable
sprite =
  { transform: { x: 0.0, y: 0.0, width: 0.0, height: 0.0 }
  , color: HtmlColor "#fff"
  , image: Nothing
  , rotation: 0.0
  , visible: true
  , id: "anon"
  , shape: Rect
  }

data EntityBehaviourResult cmd ev entity state
  = StateUpdated state
  | StateAndEntityUpdated state (Entity cmd ev entity)
  | EntityUpdated (Entity cmd ev entity)
  | RaiseEvent (Variant ev) state
  | NoOp

type BehaviourExecutionContext cmd ev entity
  = { events :: List (Variant ev)
    , scene :: (SceneSnapshot cmd ev entity)
    , entity :: (Entity cmd ev entity)
    }

type SceneSnapshot cmd ev entity
  = { entityById :: EntityId -> Maybe (Entity cmd ev entity)
    }

type EntityCommandHandlerResult cmd ev entity state
  = State (BehaviourExecutionContext cmd ev entity) state

type EntityCommandHandler cmd ev entity state
  = Variant (Cmd cmd) -> state -> (EntityCommandHandlerResult cmd ev entity state)

data EntityBehaviour cmd ev entity state
  = EntityBehaviour
    { state :: state
    , handleCommand :: EntityCommandHandler cmd ev entity state
    }

type Entity cmd ev entity
  = Record (EntityRow cmd ev entity)

type EntityRow cmd ev entity
  = ( id :: EntityId
    , location :: Point
    , rotation :: Number
    , renderables :: List Renderable
    , behaviour :: List (Exists (EntityBehaviour cmd ev entity))
    | entity
    )

type Cmd cmd
  = ( tick :: Unit | cmd )

emptyEntity :: forall cmd ev entity. EntityId -> Record entity -> Entity cmd ev entity
emptyEntity id entity =
  Record.union
    { id
    , location: { x: 0.0, y: 0.0 }
    , rotation: 0.0
    , renderables: Nil
    , behaviour: Nil
    }
    entity

processCommand :: forall cmd ev entity. (SceneSnapshot cmd ev entity) -> (Entity cmd ev entity) -> Variant (Cmd cmd) -> Tuple (Entity cmd ev entity) (List (Variant ev))
processCommand scene e command = foldr executeCommand (Tuple (e { behaviour = Nil }) Nil) e.behaviour
  where
  executeCommand :: Exists (EntityBehaviour cmd ev entity) -> (Tuple (Entity cmd ev entity) (List (Variant ev))) -> (Tuple (Entity cmd ev entity) (List (Variant ev)))
  executeCommand =
    ( \behaviour (Tuple acc evs) ->
        let
          runResult = runExists (runBehaviour acc) behaviour
        in
          Tuple (fst runResult) (concat $ (snd runResult) : evs : Nil)
    )

  runBehaviour :: forall state. Entity cmd ev entity -> EntityBehaviour cmd ev entity state -> Tuple (Entity cmd ev entity) (List (Variant ev))
  runBehaviour entity@{ behaviour: behaviourList } (EntityBehaviour behaviour@{ handleCommand: (handler) }) =
    let
      Tuple newState result = runState (handler command behaviour.state) { entity, events: Nil, scene }

      newEntity :: Entity cmd ev entity
      newEntity = result.entity

      newEvents :: List (Variant ev)
      newEvents = result.events
    in
      Tuple (newEntity { behaviour = (mkExists $ EntityBehaviour behaviour { state = newState }) : behaviourList }) newEvents
