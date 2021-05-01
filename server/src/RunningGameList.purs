module Pure.RunningGameList where

import Prelude
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Erl.Atom (atom)
import Erl.Data.List (List, (:), nil, head, filter)
import Pinto (RegistryName(..), StartLinkResult)
import Pinto.GenServer (InitResult(..), ServerPid, ServerType)
import Pinto.GenServer as Gen
import Pinto.Types (RegistryReference(..))
import Pure.Api (RunningGame)
import SimpleBus as Bus

data RunningGameBusMessage
  = GameCreated RunningGame
  | GameEnded String

foreign import generateId :: Effect String

type StartArgs
  = {}

type State
  = { knownGames :: List RunningGame }

bus :: Bus.Bus String RunningGameBusMessage
bus = Bus.bus $ "running_games_bus"

serverName :: RegistryName (ServerType Unit Unit Unit State)
serverName = Local $ atom "running_game_list"

create :: String -> String -> Boolean -> Effect String
create playerName gameName public =
  Gen.call (ByName serverName) \_from s@{ knownGames: existingGames } -> do
    id <- Gen.lift generateId
    let
      newGame = { id, startedBy: playerName, name: gameName, public }
    Gen.lift $ Bus.raise bus $ GameCreated newGame
    pure $ Gen.reply id $ s { knownGames = newGame : existingGames }

remove :: String -> Effect Unit
remove gameName =
  Gen.call (ByName serverName) \_from s@{ knownGames: existingGames } -> do
    Gen.lift $ Bus.raise bus $ GameEnded gameName
    pure $ Gen.reply unit $ s { knownGames = filter (\g -> g.id /= gameName) s.knownGames }

findAll :: Effect (List RunningGame)
findAll =
  Gen.call (ByName serverName) \_from s@{ knownGames } -> do
    pure $ Gen.reply knownGames s

findById :: String -> Effect (Maybe RunningGame)
findById id =
  Gen.call (ByName serverName) \_from s@{ knownGames } -> do
    pure $ Gen.reply (head $ filter (\g -> g.id == id) knownGames) s

startLink :: StartArgs -> Effect (StartLinkResult (ServerPid Unit Unit Unit State))
startLink args = Gen.startLink $ (Gen.defaultSpec init) { name = Just serverName }
  where
  init = pure $ InitOk { knownGames: nil }
