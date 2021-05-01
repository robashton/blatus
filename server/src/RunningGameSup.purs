module Pure.RunningGameSup where

import Prelude
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Erl.Atom (atom)
import Pinto (RegistryName(..), StartLinkResult)
import Pinto.Sup (ChildShutdownTimeoutStrategy(..), ChildType(..), RestartStrategy(..), crashIfChildNotStarted)
import Pinto.Sup.Dynamic (DynamicPid, DynamicSpec)
import Pinto.Sup.Dynamic as Sup
import Pinto.Types (RegistryReference(..))
import Pure.RunningGame (StartArgs, RunningGamePid)
import Pure.RunningGame as RunningGame
import Pure.RunningGameList as RunningGameList
import SimpleBus as Bus

serverName :: RegistryName (Sup.DynamicType StartArgs RunningGamePid)
serverName = Local $ atom $ "running_game_sup"

startLink :: Effect (StartLinkResult (DynamicPid StartArgs RunningGamePid))
startLink = Sup.startLink (Just $ Local $ atom "running_game_sup") init

init :: Effect (DynamicSpec StartArgs RunningGamePid)
init = do
  _ <-
    Bus.subscribe RunningGameList.bus
      ( \msg -> case msg of
          RunningGameList.GameCreated game -> void $ startGame { game }
          _ -> pure unit
      )
  pure
    { intensity: 1
    , period: 5
    , childType: Worker
    , start: RunningGame.startLink
    , restartStrategy: RestartTransient
    , shutdownStrategy: ShutdownTimeout 5000
    }

startGame :: RunningGame.StartArgs -> Effect RunningGamePid
startGame args = do
  crashIfChildNotStarted <$> Sup.startChild (ByName serverName) args
