module ProfileSetup where
  
import Prelude

import Data.Array as Array
import Data.Foldable (foldM, foldl)
import Data.Int as Int
import Data.Newtype (wrap, unwrap)
import Data.Tuple (Tuple(..), uncurry)
import Effect (Effect)
import Pure.Entities.Tank as Tank
import Pure.Game.Main as Main
import Pure.RunningGameList as Rgl
import Pure.Runtime.Scene (Game)
import Pure.Runtime.Scene as Scene
import Pure.Types (EntityCommand, GameEvent)

--go :: Main.State -> Main.State
--go game = foldl (\g _ -> do
--                    let result@(Tuple _ evs) = Main.tick g
----                    uncurry (foldl Main.handleEvent) result
--    
--                    ) game $ Array.range 0 1000

--setup :: Effect Main.State
--setup = 
--    foldM (\g _ -> addRandomPlayer g) Main.init $ Array.range 0 100
--
--
--addRandomPlayer :: Main.State -> Effect Main.State
--addRandomPlayer game = do
--    playerId <- Rgl.generateId
--    x <- Random.randomInt 0 10000
--    y <- Random.randomInt 0 10000
--    let player = Tank.init (wrap playerId) Tank.Server { x: Int.toNumber $ x - 5000, y: Int.toNumber $ y - 5000 }
--    pure $ game { scene = Scene.addEntity player game.scene }
