module Blatus.Client.Input where

import Prelude
import Blatus.Types (EntityCommand)
import Data.Symbol (SProxy(..))
import Data.Variant (Variant, inj)
import Effect (Effect)
import Signal (Signal, dropRepeats)
import Signal.DOM (keyPressed)
import Sisy.Types (empty)

rotateLeftSignal :: Effect (Signal (Variant EntityCommand))
rotateLeftSignal = do
  key <- keyPressed 37
  pure $ dropRepeats $ (\x -> if x then (inj (SProxy :: SProxy "turnLeft") empty) else (inj (SProxy :: SProxy "stopTurnLeft") empty)) <$> key

thrustSignal :: Effect (Signal (Variant EntityCommand))
thrustSignal = do
  key <- keyPressed 38
  pure $ dropRepeats $ (\x -> if x then (inj (SProxy :: SProxy "pushForward") empty) else (inj (SProxy :: SProxy "stopPushForward") empty)) <$> key

rotateRightSignal :: Effect (Signal (Variant EntityCommand))
rotateRightSignal = do
  key <- keyPressed 39
  pure $ dropRepeats $ (\x -> if x then (inj (SProxy :: SProxy "turnRight") empty) else (inj (SProxy :: SProxy "stopTurnRight") empty)) <$> key

brakeSignal :: Effect (Signal (Variant EntityCommand))
brakeSignal = do
  key <- keyPressed 40
  pure $ dropRepeats $ (\x -> if x then (inj (SProxy :: SProxy "pushBackward") empty) else (inj (SProxy :: SProxy "stopPushBackward") empty)) <$> key

fireSignal :: Effect (Signal (Variant EntityCommand))
fireSignal = do
  key <- keyPressed 32
  pure $ dropRepeats $ (\x -> if x then (inj (SProxy :: SProxy "startFireBullet") empty) else (inj (SProxy :: SProxy "stopFireBullet") empty)) <$> key

signal :: Effect (Signal (Variant EntityCommand))
signal = do
  fs <- fireSignal
  rl <- rotateLeftSignal
  ts <- thrustSignal
  rr <- rotateRightSignal
  bs <- brakeSignal
  pure $ fs <> rl <> ts <> rr <> bs
