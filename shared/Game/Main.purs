module Pure.Game.Main where

import Prelude

import Pure.BuiltIn.Collider as Collider
import Pure.Runtime.Control (Game)
import Pure.Runtime.Control as Control
import Pure.Types (EntityCommand(..), GameEvent(..))
import Pure.Entities.Bullet as Bullet

init :: Game EntityCommand GameEvent
init = Control.initialModel Tick
  # Control.onTick (Collider.onTick EntityCollided)

sendEvent :: Game EntityCommand GameEvent -> GameEvent ->  Game EntityCommand GameEvent
sendEvent game ev = 
  case ev of
       BulletFired deets -> 
         Control.addEntity (Bullet.init deets.id deets.location deets.velocity) game
       EntityCollided _ -> game
