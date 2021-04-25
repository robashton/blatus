module Pure.PrimarySup where

import Prelude
import Effect (Effect)
import Erl.Data.List (nil, (:))
import Data.Maybe (Maybe(..))
import Erl.Atom (atom)
import Erl.Process.Raw (class HasPid)
import Pinto (RegistryName(..), StartLinkResult)
import Pinto.Sup (ChildShutdownTimeoutStrategy(..), ChildType(..), ErlChildSpec, RestartStrategy(..), Strategy(..), SupervisorPid, SupervisorRef(..), SupervisorSpec, mkErlChildSpec)
import Pinto.Sup as Sup
import Pure.Config as PureConfig
import Pure.RunningGameList as RunningGameList
import Pure.RunningGameSup as RunningGameSup
import PureWeb as PureWeb

startLink :: Effect (StartLinkResult SupervisorPid)
startLink = do
  Sup.startLink (Just $ Local $ atom "primary_sup") init

stop :: Effect Unit
stop = do
  Sup.stop (ByName $ Local $ atom "primary_sup")

init :: Effect SupervisorSpec
init = do
  webPort <- PureConfig.webPort
  pure
    { flags:
        { strategy: OneForAll
        , intensity: 1
        , period: 5
        }
    , childSpecs:
        (spec "pure_web" $ PureWeb.startLink { webPort })
          : (spec "game_list" $ RunningGameList.startLink {})
          : (sup "game_sup" $ RunningGameSup.startLink)
          : nil
    }

spec ::
  forall childProcess.
  HasPid childProcess => String -> Effect (StartLinkResult childProcess) -> ErlChildSpec
spec id start =
  mkErlChildSpec
    { id
    , childType: Worker
    , start
    , restartStrategy: RestartOnCrash
    , shutdownStrategy: KillAfter 5000
    }

sup ::
  forall childProcess.
  HasPid childProcess => String -> Effect (StartLinkResult childProcess) -> ErlChildSpec
sup id start =
  mkErlChildSpec
    { id
    , childType: Supervisor
    , start
    , restartStrategy: RestartOnCrash
    , shutdownStrategy: KillAfter 5000
    }
