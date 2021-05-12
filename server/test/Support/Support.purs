module Test.Support where

import Prelude
import Blatus.Main as Main
import Blatus.Types (GameEvent)
import Data.Boolean (otherwise)
import Data.Exists (Exists, mkExists)
import Data.Foldable (foldl)
import Data.List (List(..), any, null)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..), fst, snd, uncurry)
import Data.Tuple.Nested (uncurry1)
import Data.Variant (class VariantMatchCases, Variant, default, inj, onMatch)
import Prim.Row as R
import Prim.RowList as RL
import Sisy.Math (point)
import Sisy.Runtime.Behaviour (raiseEvent)
import Sisy.Runtime.Behaviour as B
import Sisy.Runtime.Entity (EntityBehaviour(..), EntityId, Entity)
import Sisy.Runtime.Scene (Game)

type TestEvent
  = ( ticked :: EntityId )

ticked :: EntityId -> Variant TestEvent
ticked = inj (SProxy :: SProxy "ticked")

runWhileEvents :: Tuple Main.State (List (Variant GameEvent)) -> Main.State
runWhileEvents (Tuple state evs)
  | null evs = state
  | otherwise = runWhileEvents $ foldl (\acc ev -> uncurry (\ng nevs -> Tuple ng $ (snd acc) <> nevs) $ Main.handleEvent (fst acc) ev) (Tuple state Nil) evs

emptyEntity :: EntityId -> Entity () TestEvent ()
emptyEntity =
  { id: _
  , location: point 0.0 0.0
  , rotation: 0.0
  , behaviour: Nil
  , renderables: Nil
  }

tickEcho :: forall entity. Exists (EntityBehaviour () TestEvent entity)
tickEcho =
  mkExists
    $ EntityBehaviour
        { state: {}
        , handleCommand:
            ( \cmd state ->
                onMatch
                  { tick:
                      \_ -> do
                        id <- B.id
                        raiseEvent $ ticked id
                        pure state
                  }
                  (default (pure state))
                  cmd
            )
        }

entityExists ::
  forall cmd ev entity.
  (Entity cmd ev entity -> Boolean) -> Game cmd ev entity -> Boolean
entityExists f game = any f game.entities

eventExists ::
  forall r rl r1 r2 r3.
  RL.RowToList r rl =>
  VariantMatchCases rl r1 Boolean =>
  R.Union r1 r2 r3 =>
  Record r ->
  List (Variant r3) -> Boolean
eventExists r = any (default false # onMatch r)

--findEvent ::
--  forall r rl r1 r2 result m.
--  RL.RowToList r rl =>
--  VariantMatchCases rl r1 (m result) =>
--  Alternative m =>
--  R.Union r1 r2 GameEvent =>
--  Record r ->
--  List (Variant GameEvent) -> (m result)
--findEvent r = any (default empty # onMatch r)
--
--        finalState
--
--        playerSpawned = 
--      assertEqual
--        { expected: Nil
--        , actual: newState.pendingSpawns
--        }
--      assertEqual
--        { expected: true
--        , actual: playerSpawned
--        }
