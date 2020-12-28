module Pure.RunningGameSup where

import Effect
import Erl.Data.List
import Prelude

import Erl.Atom (atom)
import Erl.Process.Raw (Pid)
import Pinto as Pinto
import Pinto  (ServerName(..) ,SupervisorName(..))
import Effect.Exception (throw)
import Pinto.Sup (startLink, startSimpleChild) as Sup
import Pinto.Sup 
import PureWeb as PureWeb
import Pure.Config as PureConfig
import Pure.RunningGameList as RunningGameList
import Pure.RunningGame as RunningGame
import SimpleBus as Bus

serverName :: SupervisorName
serverName = (Local $ atom "game_sup")

startLink :: Unit ->  Effect Pinto.StartLinkResult
startLink _ = Sup.startLink serverName init


init :: Effect SupervisorSpec
init = do
  void $ Bus.subscribe RunningGameList.bus  (\msg ->
    case msg of
         RunningGameList.GameCreated game ->
           void $ startGame { game }
         _ ->
           pure unit

    )
  pure $ buildSupervisor
    # supervisorStrategy SimpleOneForOne 
    # supervisorIntensity 100
    # supervisorPeriod 60
    # supervisorChildren (( buildChild
                         # childType Worker
                         # childId "running_game_child"
                         # childRestart Transient
                         # childStartTemplate childTemplate
                         ) : nil)

childTemplate :: Pinto.ChildTemplate RunningGame.StartArgs
childTemplate = Pinto.ChildTemplate (RunningGame.startLink)

startGame :: RunningGame.StartArgs -> Effect Pid
startGame args = do
  result <- Sup.startSimpleChild childTemplate serverName args
  case result of
       Pinto.ChildAlreadyStarted pid -> pure pid
       Pinto.ChildStarted pid -> pure pid
       something -> do
          throw "Failed to start game"
