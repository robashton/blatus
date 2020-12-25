module Pure.Game.Main where

import Prelude

import Data.Array (fromFoldable)
import Data.Array as Array
import Data.Bifunctor (lmap, rmap)
import Data.Foldable (foldl)
import Data.List (List(..))
import Data.List (toUnfoldable)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), fst, snd, uncurry)
import Pure.BuiltIn.Bullets as Bullets
import Pure.BuiltIn.Collider as Collider
import Pure.BuiltIn.Explosions as Explosions
import Pure.Comms (GameSync, EntitySync)
import Pure.Entities.Bullet as Bullet
import Pure.Entities.Tank as Tank
import Pure.Entity (Entity, EntityClass(..), EntityId(..))
import Pure.Runtime.Scene (Game)
import Pure.Runtime.Scene as Scene
import Pure.Runtime.Ticks as Ticks
import Pure.Types (EntityCommand(..), GameEvent(..))

timePerFrame :: Number
timePerFrame = 1000.0 / 30.0

type RegisteredPlayer = { id :: EntityId
                        , lastTick :: Int
                        , score :: Int
                        }


type State = { scene :: Game EntityCommand GameEvent
             , bullets :: Bullets.State GameEvent
             , explosions :: Explosions.State
             , players :: Map.Map EntityId RegisteredPlayer
             , lastTick :: Int
             , ticks :: Ticks.State
             }

init :: Number -> State 
init now = { scene: Scene.initialModel Tick
             #  Scene.onTick (Collider.onTick EntityCollided)
             , bullets: Bullets.init BulletHit
             , explosions: Explosions.init
             , lastTick: 0
             , ticks: Ticks.init now timePerFrame
             , players: mempty
             }

tick :: Number -> State -> Tuple State (List GameEvent)
tick now state = 
  lmap (\newState -> newState { ticks = newTicks }) result
  where
        (Tuple framesToExecute newTicks) = Ticks.update now state.ticks
        result = foldl (\acc x -> if x == 0 then acc else innerTick acc) (Tuple state Nil) $ Array.range 0 framesToExecute
        innerTick (Tuple is evs) = rmap (\nevs -> (evs <> nevs)) $ doTick is

doTick :: State -> Tuple State (List GameEvent)
doTick state@{ lastTick } =
  Tuple (state{ scene = fst sceneTick
              , bullets = fst bulletTick
              , explosions = explosionTick
              , lastTick = lastTick + 1 
              }) (snd sceneTick <> snd bulletTick)
  where
    explosionTick = Explosions.tick state.explosions
    bulletTick = Bullets.tick state.bullets (fst sceneTick)
    sceneTick = Scene.tick state.scene

handleEvent :: State -> GameEvent -> Tuple State (List GameEvent)
handleEvent state@{ scene } ev = 
  case ev of
       BulletFired deets -> 
         Tuple (state { bullets = Bullets.fireBullet deets.id deets.location deets.velocity deets.power state.bullets }) Nil

       EntityDestroyed id -> 
         Tuple (state { scene = Scene.removeEntity id scene })  Nil

       BulletHit hit ->
         lmap (\s -> state { scene = s
                           , explosions = explosions
                           }) $ Scene.sendCommand hit.entity (Damage hit.bullet.power) scene
         where
               explosions = Explosions.createExplosion hit.entity hit.bullet.location hit.bullet.velocity state.explosions 
       EntityCollided _ -> 
         Tuple state Nil

sendCommand :: EntityId -> EntityCommand -> State -> Tuple State (List GameEvent)
sendCommand id cmd state = 
  lmap (\s -> (state { scene = s })) $ Scene.sendCommand id cmd state.scene

fromSync :: Number -> GameSync -> State 
fromSync now { entities, world, players, tick: t } = 
  let state = init now
   in
  state { scene = state.scene { world = world
                              , entities = foldl (\m e -> Map.insert e.id (entityFromSync e) m) mempty entities
                              }
        , players = foldl (\m p -> Map.insert p.id p m) mempty players
        , lastTick = t
        }

toSync :: State -> GameSync
toSync state@{ players, scene: { entities, world }, lastTick } = { entities: toUnfoldable $ map entityToSync $ Map.values entities
                                                            , world
                                                            , tick: lastTick
                                                            , players: fromFoldable $ Map.values players
                                                            -- Bullets go here
                                                            }

entityToSync :: forall cmd ev. Entity cmd ev -> EntitySync
entityToSync { id, class: c, location, velocity, rotation } =
  { id, class: c, location, velocity, rotation }

addPlayer :: EntityId -> Number -> Number -> State -> Tuple State EntitySync
addPlayer id x y state@{ scene, players, lastTick } = 
  Tuple (state { scene = Scene.addEntity player scene
               , players = Map.insert id { id, lastTick, score: 0 } players 
               }) $ entityToSync player
  where
        player = Tank.init id Tank.Server { x, y }

updatePlayerTick :: EntityId -> Int -> State -> State
updatePlayerTick id t state@{ players } = 
  state { players = Map.update (\v -> Just $ v { lastTick = t }) id players }

removePlayer :: EntityId -> State -> State
removePlayer id state@{ players } = 
  removeEntity id state { players = Map.delete id players }

addEntity :: EntitySync -> State -> State 
addEntity sync state =
  state { scene = Scene.addEntity (entityFromSync sync) state.scene }
  where entity = case sync.class of
                    Tank -> Tank.init sync.id Tank.Client sync.location
                    Bullet -> Bullet.init sync.id sync.location sync.velocity

removeEntity :: EntityId -> State -> State 
removeEntity id state =
  state { scene = Scene.removeEntity id state.scene }

entityFromSync :: EntitySync -> Entity EntityCommand GameEvent
entityFromSync sync =
  let blank = case sync.class of
                Tank -> Tank.init sync.id Tank.Client sync.location
                Bullet -> Bullet.init sync.id sync.location sync.velocity
   in
   blank { location = sync.location
         , velocity = sync.velocity
         , rotation = sync.rotation }

mergeSyncInfo :: State -> GameSync -> State
mergeSyncInfo state@{ scene } sync =
  state { scene = foldl (\acc es -> Scene.discardEvents $ Scene.sendCommand es.id (UpdateServerState { location: es.location
                                                                                         , velocity: es.velocity
                                                                                         , rotation: es.rotation 
                                                                                         }) acc
    ) scene sync.entities }


mergePlayerSync :: State -> EntitySync -> State
mergePlayerSync state es =
  state { scene = Scene.discardEvents $ Scene.sendCommand es.id (UpdateServerState { location: es.location
                                                                 , velocity: es.velocity
                                                                 , rotation: es.rotation 
                                                                 }) state.scene }
                        

