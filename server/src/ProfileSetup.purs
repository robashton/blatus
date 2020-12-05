module ProfileSetup where
  
import Prelude
import Data.Tuple (Tuple(..), uncurry)
import Data.Foldable (foldM, foldl)
import Effect (Effect)
import Data.Int as Int
import Data.Array as Array
import Effect.Random as Random
import Pure.Game (Game)
import Data.Newtype (wrap, unwrap)
import Pure.RunningGameList as Rgl

import Pure.Game as Game

go :: Game -> Game
go game = foldl (\g _ -> do
                    let result@(Tuple _ evs) = Game.tick g
                    uncurry Game.foldEvents result
                    ) game $ Array.range 0 1000

setup :: Effect Game
setup = 
    foldM (\g _ -> addRandomPlayer g) Game.initialModel $ Array.range 0 100


addRandomPlayer :: Game -> Effect Game
addRandomPlayer game = do
    playerId <- Rgl.generateId
    x <- Random.randomInt 0 10000
    y <- Random.randomInt 0 10000
    let player = Game.tank (wrap playerId) Game.Server { x: Int.toNumber $ x - 5000, y: Int.toNumber $ y - 5000 }
    pure $ Game.addEntity player game
