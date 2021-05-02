module Blatus.Server.PrimarySup where

import Prelude
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Erl.Atom (atom)
import Erl.Data.List (nil, (:))
import Erl.Process.Raw (class HasPid)
import Pinto (RegistryName(..), StartLinkResult)
import Pinto.Sup (ChildShutdownTimeoutStrategy(..), ChildType(..), ErlChildSpec, RestartStrategy(..), Strategy(..), SupervisorPid, SupervisorSpec, spec)
import Pinto.Sup as Sup
import Pinto.Types (RegistryReference(..))
import Blatus.Server.Config as Config
import Blatus.Server.RunningGameList as RunningGameList
import Blatus.Server.RunningGameSup as RunningGameSup
import Blatus.Server.Web as Web

startLink :: Effect (StartLinkResult SupervisorPid)
startLink = do
  Sup.startLink (Just $ Local $ atom "primary_sup") init

stop :: Effect Unit
stop = do
  Sup.stop (ByName $ Local $ atom "primary_sup")

init :: Effect SupervisorSpec
init = do
  webPort <- Config.webPort
  pure
    { flags:
        { strategy: OneForAll
        , intensity: 1
        , period: 5
        }
    , childSpecs:
        (worker "pure_web" $ Web.startLink { webPort })
          : (worker "game_list" $ RunningGameList.startLink {})
          : (sup "game_sup" $ RunningGameSup.startLink)
          : nil
    }

worker ::
  forall childProcess.
  HasPid childProcess => String -> Effect (StartLinkResult childProcess) -> ErlChildSpec
worker id start =
  spec
    { id
    , childType: Worker
    , start
    , restartStrategy: RestartTransient
    , shutdownStrategy: ShutdownTimeout 5000
    }

sup ::
  forall childProcess.
  HasPid childProcess => String -> Effect (StartLinkResult childProcess) -> ErlChildSpec
sup id start =
  spec
    { id
    , childType: Supervisor
    , start
    , restartStrategy: RestartTransient
    , shutdownStrategy: ShutdownInfinity
    }
