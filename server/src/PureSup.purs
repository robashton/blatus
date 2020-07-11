module PureSup where

import Effect
import Erl.Data.List
import Prelude

import Erl.Atom (atom)
import Pinto as Pinto
import Pinto  (ServerName(..) ,SupervisorName(..))
import Pinto.Sup (startLink) as Sup
import Pinto.Sup 
import PureWeb as PureWeb
import PureConfig as PureConfig

serverName :: SupervisorName
serverName = (Local $ atom "pure_sup")

startLink :: Effect Pinto.StartLinkResult
startLink = Sup.startLink serverName init

init :: Effect SupervisorSpec
init = do
  webPort <- PureConfig.webPort
  pure $ buildSupervisor
                # supervisorStrategy OneForOne
                # supervisorChildren ( ( buildChild
                                       # childType Worker
                                       # childId "pure_web"
                                       # childStart PureWeb.startLink  { webPort } )
                                        : nil)


