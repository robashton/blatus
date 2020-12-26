module Pure.Game.Main where

import Prelude

import Data.Array (fromFoldable)
import Data.Array as Array
import Data.Bifunctor (lmap, rmap)
import Data.Filterable (filter)
import Data.Foldable (foldl)
import Data.List (List(..), head, (:))
import Data.List (toUnfoldable)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
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
import Pure.Types (EntityCommand(..), GameEvent(..), RegisteredPlayer)

timePerFrame :: Number
timePerFrame = 1000.0 / 30.0

foreign import data Seed :: Type
foreign import seed :: Seed
foreign import random :: (Number -> Seed -> Tuple Number Seed) -> Seed -> Tuple Number Seed


type State = { scene :: Game EntityCommand GameEvent
             , bullets :: Bullets.State GameEvent
             , explosions :: Explosions.State
             , players :: Map.Map EntityId RegisteredPlayer
             , lastTick :: Int
             , ticks :: Ticks.State
             , pendingSpawns :: List { ticks :: Int, playerId :: EntityId }
             , seed :: Seed
             }
pendingSpawn :: EntityId -> State -> Maybe Int
pendingSpawn id state = 
  map _.ticks $ head $ filter (\x -> x.playerId == id) state.pendingSpawns

init :: Number -> State 
init now = { scene: Scene.initialModel Tick
             #  Scene.onTick (Collider.onTick EntityCollided)
             , bullets: Bullets.init BulletHit
             , explosions: Explosions.init
             , lastTick: 0
             , ticks: Ticks.init now timePerFrame
             , players: mempty
             , pendingSpawns: mempty
             , seed: seed
             }

tick :: Number -> State -> Tuple State (List GameEvent)
tick now state = 
  lmap (\newState -> newState { ticks = newTicks }) result
  where
        (Tuple framesToExecute newTicks) = Ticks.update now state.ticks
        result = foldl (\acc x -> if x == 0 then acc else innerTick acc) (Tuple state Nil) $ Array.range 0 framesToExecute
        innerTick (Tuple is evs) = rmap (\nevs -> (evs <> nevs)) $ doTick is

doTick :: State -> Tuple State (List GameEvent)
doTick state@{ lastTick, seed } =
  Tuple (state{ scene = fst sceneTick
              , bullets = fst bulletTick
              , explosions = explosionTick
              , lastTick = lastTick + 1 
              , pendingSpawns = fst spawnTick
              , seed = fst processedSpawns
              }) (snd sceneTick <> snd bulletTick <> snd processedSpawns)
  where
    explosionTick = Explosions.tick state.explosions
    bulletTick = Bullets.tick state.bullets (fst sceneTick)
    sceneTick = Scene.tick state.scene
    spawnTick = foldl (\acc s -> if s.ticks <= 0 then rmap (\evs -> s.playerId : evs) acc
                                 else lmap (\ts -> s { ticks = s.ticks - 1 } : ts) acc)
                      (Tuple Nil Nil) state.pendingSpawns
    processedSpawns = foldl (\acc id -> 
                        let a@(Tuple x s1) = random Tuple $ fst acc
                            b@(Tuple y s2) = random Tuple s1
                         in
                           (Tuple s2 $ (PlayerSpawn { x: x * 1000.0 - 500.0, y: y * 1000.0 - 500.0, id }) : (snd acc))
                      ) (Tuple state.seed Nil) (snd spawnTick)


handleEvent :: State -> GameEvent -> Tuple State (List GameEvent)
handleEvent state@{ scene } ev = 
  case ev of
       BulletFired deets -> 
         Tuple (state { bullets = Bullets.fireBullet deets.owner deets.location deets.velocity deets.power state.bullets }) Nil

       EntityDestroyed { entity: id, destroyer } -> 
         Tuple (handleEntityDestruction state id destroyer) Nil

       PlayerSpawn { id, x, y } -> 
         Tuple (state { scene = Scene.addEntity (Tank.init id { x, y }) scene })  Nil

       BulletHit hit ->
         lmap (\s -> state { scene = s
                           , explosions = explosions
                           }) $ Scene.sendCommand hit.entity (Damage { amount: hit.bullet.power,  source: Just hit.bullet.owner }) scene
         where
               explosions = Explosions.createExplosion hit.entity hit.bullet.location hit.bullet.velocity state.explosions 
       EntityCollided _ -> 
         Tuple state Nil

handleEntityDestruction :: State -> EntityId -> Maybe EntityId -> State
handleEntityDestruction state@{ scene, players, pendingSpawns } id destroyer = 
  state { scene = Scene.removeEntity id scene 
        , players=  maybe players (\d -> Map.update (\player -> Just $ player { score = player.score + 1 }) d players) destroyer
        , pendingSpawns = { playerId: id, ticks: 300 } : pendingSpawns
        }

          
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

addPlayer :: EntityId -> State -> State
addPlayer id state@{ players, lastTick, pendingSpawns } = 
  state { players = Map.insert id { id, lastTick, score: 0 } players
        , pendingSpawns = { playerId: id, ticks: 0 } : pendingSpawns
        }



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
                    Tank -> Tank.init sync.id sync.location
                    Bullet -> Bullet.init sync.id sync.location sync.velocity

removeEntity :: EntityId -> State -> State 
removeEntity id state =
  state { scene = Scene.removeEntity id state.scene }

entityFromSync :: EntitySync -> Entity EntityCommand GameEvent
entityFromSync sync =
  let blank = case sync.class of
                Tank -> Tank.init sync.id sync.location
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
                        

