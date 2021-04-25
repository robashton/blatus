module Pure.RunningGameSup where

import Prelude
import Effect (Effect)
import Data.Maybe (Maybe(..))
import Erl.Atom (atom)
import Pinto (RegistryName(..), StartLinkResult)
import Pinto.Sup (ChildShutdownTimeoutStrategy(..), ChildType(..), RestartStrategy(..), crashIfChildNotStarted)
import Pinto.Sup.Dynamic (DynamicPid, DynamicSpec)
import Pinto.Sup.Dynamic as Sup
import Pure.RunningGame (StartArgs, RunningGamePid)
import Pure.RunningGame as RunningGame

serverName :: RegistryName (Sup.DynamicType StartArgs RunningGamePid)
serverName = Local $ atom $ "running_game_sup"

startLink :: Effect (StartLinkResult (DynamicPid StartArgs RunningGamePid))
startLink = Sup.startLink (Just $ Local $ atom "running_game_sup") init

init :: Effect (DynamicSpec StartArgs RunningGamePid)
init =
  pure
    { intensity: 1
    , period: 5
    , childType: Worker
    , start: RunningGame.startLink
    , restartStrategy: RestartOnCrash
    , shutdownStrategy: KillAfter 5000
    }

startGame :: RunningGame.StartArgs -> Effect RunningGamePid
startGame args = do
  crashIfChildNotStarted <$> Sup.startChild args (Sup.ByName serverName)
