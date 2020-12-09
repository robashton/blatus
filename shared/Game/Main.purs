module Pure.Game.Main where

import Prelude

import Data.Array (foldl)
import Data.Bifunctor (lmap)
import Data.List (List(..))
import Data.List (toUnfoldable)
import Data.Map as Map
import Data.Tuple (Tuple(..), fst, snd)
import Pure.BuiltIn.Bullets as Bullets
import Pure.BuiltIn.Collider as Collider
import Pure.BuiltIn.Explosions as Explosions
import Pure.Comms (GameSync, EntitySync)
import Pure.Entities.Bullet as Bullet
import Pure.Entities.Tank as Tank
import Pure.Entity (Entity, EntityClass(..), EntityId(..))
import Pure.Runtime.Scene (Game)
import Pure.Runtime.Scene as Scene
import Pure.Types (EntityCommand(..), GameEvent(..))

type State = { scene :: Game EntityCommand GameEvent
             , bullets :: Bullets.State GameEvent
             , explosions :: Explosions.State
             }

init :: State 
init = { scene: Scene.initialModel Tick
             #  Scene.onTick (Collider.onTick EntityCollided)
       , bullets: Bullets.init BulletHit
       , explosions: Explosions.init
       }

tick :: State -> Tuple State (List GameEvent)
tick state = 
  Tuple (state{ scene = fst sceneTick
              , bullets = fst bulletTick
              , explosions = explosionTick
              }) (snd sceneTick <> snd bulletTick)
  where
  explosionTick = Explosions.tick state.explosions
  bulletTick = Bullets.tick state.bullets (fst sceneTick)
  sceneTick = Scene.tick state.scene

handleEvent :: State -> GameEvent -> State
handleEvent state@{ scene } ev = 
  case ev of
       BulletFired deets -> 
         state { bullets = Bullets.fireBullet deets.id deets.location deets.velocity state.bullets }
       BulletHit hit ->
         -- send command in to damage entity
         -- add an explosion
         state { explosions = Explosions.createExplosion hit.entity hit.bullet.location hit.bullet.velocity state.explosions }
       EntityCollided _ -> 
         state

sendCommand :: EntityId -> EntityCommand -> State -> Tuple State (List GameEvent)
sendCommand id cmd state = 
  lmap (\s -> (state { scene = s })) $ Scene.sendCommand id cmd state.scene

fromSync :: GameSync -> State 
fromSync { entities, world } = 
  let state = init 
   in
  state { scene = state.scene { world = world
                              , entities = foldl (\m e -> Map.insert e.id (entityFromSync e) m) mempty entities
                             }}

toSync :: State -> Int -> GameSync
toSync state@{ scene: { entities, world } } tick = { entities: toUnfoldable $ map entityToSync $ Map.values entities
                                                            , world
                                                            , tick
                                                            -- Bullets go here
                                                            }

entityToSync :: forall cmd ev. Entity cmd ev -> EntitySync
entityToSync { id, class: c, location, velocity, rotation } =
  { id, class: c, location, velocity, rotation }

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
                        

