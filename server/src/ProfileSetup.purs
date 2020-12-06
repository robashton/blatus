module ProfileSetup where
  
import Prelude

import Data.Array as Array
import Data.Foldable (foldM, foldl)
import Data.Int as Int
import Data.Newtype (wrap, unwrap)
import Data.Tuple (Tuple(..), uncurry)
import Effect (Effect)
import Effect.Random as Random
import Pure.Entities.Tank as Tank
import Pure.Game.Main as Main
import Pure.RunningGameList as Rgl
import Pure.Runtime.Control (Game)
import Pure.Runtime.Control as Control
import Pure.Types (EntityCommand, GameEvent)

go :: Game EntityCommand GameEvent -> Game EntityCommand GameEvent
go game = foldl (\g _ -> do
                    let result@(Tuple _ evs) = Control.tick g
                    uncurry (foldl Main.sendEvent) result
                    ) game $ Array.range 0 1000

setup :: Effect (Game EntityCommand GameEvent) 
setup = 
    foldM (\g _ -> addRandomPlayer g) Main.init $ Array.range 0 100


addRandomPlayer :: Game EntityCommand GameEvent -> Effect (Game EntityCommand GameEvent)
addRandomPlayer game = do
    playerId <- Rgl.generateId
    x <- Random.randomInt 0 10000
    y <- Random.randomInt 0 10000
    let player = Tank.init (wrap playerId) Tank.Server { x: Int.toNumber $ x - 5000, y: Int.toNumber $ y - 5000 }
    pure $ Control.addEntity player game
