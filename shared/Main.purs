module Blatus.Main where

import Prelude
import Blatus.BuildMenu (BuildAction, BuildActionInfo)
import Blatus.BuildMenu as BuildMenu
import Blatus.Comms (GameSync, EntitySync)
import Blatus.Entities (CollectableType(..), EntityClass(..))
import Blatus.Entities.Asteroid as Asteroid
import Blatus.Entities.Collectable as Collectable
import Blatus.Entities.Tank as Tank
import Blatus.Entities.Turret as Turret
import Blatus.Types (BuildTemplate(..), EntityCommand, GameEntity, GameEvent, RegisteredPlayer, buildRequested, playerSpawn)
import Control.Apply (lift2)
import Data.Array (fromFoldable)
import Data.Array as Array
import Data.Bifunctor (lmap, rmap)
import Data.Filterable (filter, filterMap)
import Data.Foldable (foldl)
import Data.List (List(..), head, (:), toUnfoldable)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..), fst, snd)
import Data.Variant (Variant, expand, inj, match)
import Debug (spy)
import Sisy.BuiltIn (impact, damage)
import Sisy.BuiltIn.Extensions.Bullets as Bullets
import Sisy.BuiltIn.Extensions.Collider as Collider
import Sisy.BuiltIn.Extensions.Explosions as Explosions
import Sisy.Math (origin)
import Sisy.Runtime.Entity (Cmd, Entity, EntityId(..))
import Sisy.Runtime.Scene (Game)
import Sisy.Runtime.Scene as Scene
import Sisy.Runtime.Ticks as Ticks

timePerFrame :: Number
timePerFrame = 1000.0 / 30.0

ticksForRespawn :: Int
ticksForRespawn = 300

foreign import data Seed :: Type

foreign import seed :: Seed

foreign import random :: (Number -> Seed -> Tuple Number Seed) -> Seed -> Tuple Number Seed

type State
  = { scene :: Game EntityCommand GameEvent GameEntity
    , bullets :: Bullets.State
    , explosions :: Explosions.State
    , players :: Map.Map EntityId RegisteredPlayer
    , lastTick :: Int
    , ticks :: Ticks.State
    , pendingSpawns :: List { ticks :: Int, playerId :: EntityId }
    , buildActions :: Map.Map BuildTemplate BuildAction
    , seed :: Seed
    }

pendingSpawn :: EntityId -> State -> Maybe Int
pendingSpawn id state = map _.ticks $ head $ filter (\x -> x.playerId == id) state.pendingSpawns

buildAction :: BuildTemplate -> EntityId -> State -> Maybe BuildActionInfo
buildAction template playerId { scene, buildActions, players } =
  let
    player = Map.lookup playerId players
  in
    Map.lookup template buildActions
      >>= (\action -> (action.info <$> player) <*> pure scene)

playerBuildActions :: EntityId -> State -> List BuildActionInfo
playerBuildActions playerId state =
  let
    player = Map.lookup playerId state.players
  in
    filterMap
      (\action -> ((action.info <$> player) <*> pure state.scene))
      $ Map.values state.buildActions

init :: Number -> State
init now =
  { scene:
      Scene.initialModel
        # Scene.onTick Collider.onTick
  , bullets: Bullets.init
  , explosions: Explosions.init
  , lastTick: 0
  , ticks: Ticks.init now timePerFrame
  , players: Map.empty
  , pendingSpawns: mempty
  , buildActions: foldl (\m i -> Map.insert i.template i m) Map.empty $ BuildMenu.actions
  , seed: seed
  }

tick :: Number -> State -> Tuple State (List (Variant GameEvent))
tick now state = lmap (\newState -> newState { ticks = newTicks }) result
  where
  (Tuple framesToExecute newTicks) = Ticks.update now state.ticks

  result = foldl (\acc x -> if x == 0 then acc else innerTick acc) (Tuple state Nil) $ Array.range 0 framesToExecute

  innerTick (Tuple is evs) = rmap (\nevs -> (evs <> nevs)) $ doTick is

doTick :: State -> Tuple State (List (Variant GameEvent))
doTick state@{ lastTick } =
  Tuple
    ( state
        { scene = fst sceneTick
        , bullets = fst bulletTick
        , explosions = explosionTick
        , lastTick = lastTick + 1
        , pendingSpawns = fst spawnTick
        , seed = fst processedSpawns
        }
    )
    (snd sceneTick <> bulletEvents <> (snd processedSpawns))
  where
  explosionTick = Explosions.tick state.explosions

  bulletEvents :: List (Variant GameEvent)
  bulletEvents = expand <$> snd bulletTick

  bulletTick = Bullets.tick state.bullets (fst sceneTick)

  sceneTick = Scene.tick state.scene

  spawnTick =
    foldl
      ( \acc s ->
          if s.ticks <= 0 then
            rmap (\evs -> s.playerId : evs) acc
          else
            lmap (\ts -> s { ticks = s.ticks - 1 } : ts) acc
      )
      (Tuple Nil Nil)
      state.pendingSpawns

  processedSpawns =
    foldl
      ( \acc id ->
          let
            a@(Tuple x s1) = random Tuple $ fst acc

            b@(Tuple y s2) = random Tuple s1
          in
            (Tuple s2 $ (playerSpawn { x: x * 1000.0 - 500.0, y: y * 1000.0 - 500.0, id }) : (snd acc))
      )
      (Tuple state.seed Nil)
      (snd spawnTick)

handleEvent :: State -> Variant GameEvent -> Tuple State (List (Variant GameEvent))
handleEvent state@{ scene, players, buildActions } =
  match
    { bulletFired: \deets -> Tuple (state { bullets = Bullets.fireBullet deets.owner deets.location deets.velocity deets.power state.bullets }) Nil
    , entityDestroyed:
        \{ entity: id, destroyer } -> case Map.lookup id players of
          Nothing -> Tuple (state { scene = Scene.removeEntity id scene }) Nil
          Just _player -> Tuple (handleEntityDestruction state id destroyer) Nil
    , playerSpawn: \{ id, x, y } -> Tuple (state { scene = Scene.addEntity (Tank.init id { x, y }) scene }) Nil
    , bulletHit:
        \hit ->
          let
            explosions = Explosions.createExplosion hit.entity hit.bullet.location hit.bullet.velocity state.explosions
          in
            lmap
              ( \s ->
                  state
                    { scene = s
                    , explosions = explosions
                    }
              )
              $ Scene.sendCommand hit.entity (damage { amount: hit.bullet.power, location: hit.bullet.location, source: Just hit.bullet.owner }) scene
    , entityCollided: \ev -> lmap (\s -> state { scene = s }) $ Scene.sendCommand ev.left (impact { force: ev.force, source: ev.right }) scene
    , resourceProvided:
        ( \ev ->
            let
              updatePlayer p = case ev.resource of
                Rock amount -> Just $ p { availableRock = p.availableRock + amount }
            in
              Tuple (state { players = Map.update updatePlayer ev.to players }) Nil
        )
    , collectableSpawned: \ev -> Tuple (state { scene = Scene.addEntity ((Collectable.init ev.id ev.location ev.args) { velocity = ev.velocity }) scene }) Nil
    , buildRequested:
        \ev ->
          let
            mp = spy "player" $ Map.lookup ev.entity players

            ma = spy "action" $ Map.lookup ev.template buildActions

            entityClass =
              join
                $ lift2
                    ( \action player ->
                        let
                          info = action.info player state.scene
                        in
                          if info.available then (action.get ev.location player state.scene) else Nothing
                    )
                    ma
                    mp

            sync =
              spy "sync"
                { id: ev.id
                , location: ev.location
                , velocity: origin
                , class: _
                , rotation: 0.0
                , health: 0.0
                , shield: 0.0
                }
                <$> entityClass

            next = maybe state (\s -> addEntity s state) sync
          in
            Tuple next Nil
    }

handleEntityDestruction :: State -> EntityId -> Maybe EntityId -> State
handleEntityDestruction state@{ scene, players, pendingSpawns } id destroyer =
  state
    { scene = Scene.removeEntity id scene
    , players = maybe players (\d -> Map.update (\player -> Just $ player { score = player.score + 1 }) d players) destroyer
    , pendingSpawns = { playerId: id, ticks: ticksForRespawn } : pendingSpawns
    }

sendCommand :: EntityId -> Variant (Cmd EntityCommand) -> State -> Tuple State (List (Variant GameEvent))
sendCommand id cmd state = lmap (\s -> (state { scene = s })) $ Scene.sendCommand id cmd state.scene

fromSync :: Number -> GameSync -> State
fromSync now { entities, world, players, tick: t } =
  let
    state = init now
  in
    state
      { scene =
        state.scene
          { world = world
          , entities = foldl (\m e -> Map.insert e.id (entityFromSync e) m) Map.empty entities
          }
      , players = foldl (\m p -> Map.insert p.id p m) Map.empty players
      , lastTick = t
      }

toSync :: State -> GameSync
toSync state@{ players, scene: { entities, world }, lastTick } =
  { entities: toUnfoldable $ map entityToSync $ Map.values entities
  , world
  , tick: lastTick
  , players: fromFoldable $ Map.values players
  }

entityToSync :: forall cmd ev. Entity cmd ev GameEntity -> EntitySync
entityToSync { id, class: c, location, velocity, rotation, health, shield } = { id, class: c, location, velocity, rotation, health, shield }

addPlayer :: EntityId -> State -> State
addPlayer id state@{ players, lastTick, pendingSpawns } =
  state
    { players = Map.insert id { id, lastTick, score: 0, availableRock: 0 } players
    , pendingSpawns = { playerId: id, ticks: 0 } : pendingSpawns
    }

updatePlayerTick :: EntityId -> Int -> State -> State
updatePlayerTick id t state@{ players } = state { players = Map.update (\v -> Just $ v { lastTick = t }) id players }

removePlayer :: EntityId -> State -> State
removePlayer id state@{ players } = removeEntity id state { players = Map.delete id players }

addEntity :: EntitySync -> State -> State
addEntity sync state = state { scene = Scene.addEntity (entityFromSync sync) state.scene }

removeEntity :: EntityId -> State -> State
removeEntity id state = state { scene = Scene.removeEntity id state.scene }

entityFromSync :: EntitySync -> Entity EntityCommand GameEvent GameEntity
entityFromSync sync =
  let
    blank = case sync.class of
      Tank -> Tank.init sync.id sync.location
      Asteroid { width, height } -> Asteroid.init sync.id sync.location width height
      Collectable args -> Collectable.init sync.id sync.location args
      Turret args -> Turret.init sync.id sync.location args
  in
    blank
      { location = sync.location
      , velocity = sync.velocity
      , rotation = sync.rotation
      , shield = sync.shield
      , health = sync.health
      }

mergeSyncInfo :: State -> GameSync -> State
mergeSyncInfo state@{ scene } sync =
  state
    { scene =
      foldl
        ( \acc es ->
            Scene.discardEvents
              $ Scene.sendCommand es.id
                  ( inj (SProxy :: SProxy "updateServerState")
                      { location: es.location
                      , velocity: es.velocity
                      , rotation: es.rotation
                      }
                  )
                  acc
        )
        scene
        sync.entities
    }

mergePlayerSync :: State -> EntitySync -> State
mergePlayerSync state es =
  state
    { scene =
      Scene.discardEvents
        $ Scene.sendCommand es.id
            ( inj (SProxy :: SProxy "updateServerState")
                { location: es.location
                , velocity: es.velocity
                , rotation: es.rotation
                }
            )
            state.scene
    }
