module Pure.RunningGameList where
 
import Prelude
import Effect (Effect)
import Erl.Atom (atom)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap, unwrap)
import Effect (Effect)
import Erl.Data.List (List, (:), nil, head, filter)
import Pinto (ServerName(..), StartLinkResult)
import Pinto.Gen (CallResult(..))
import Pinto.Gen as Gen
import Pure.Api (RunningGame(..))
import SimpleBus as Bus

data RunningGameBusMessage = GameCreated RunningGame

foreign import generateId :: Effect String

type StartArgs = {}
type State = { knownGames :: List RunningGame }

bus :: Bus.Bus String RunningGameBusMessage
bus = Bus.bus $ "running_games_bus"

serverName :: ServerName State Unit
serverName = Local $ atom "running_game_list"

create :: String -> String -> Boolean -> Effect String
create playerName gameName public = 
  Gen.call serverName \s@{ knownGames: existingGames } -> do
     id <- Gen.lift generateId
     let newGame = { id, startedBy: playerName, name: gameName, public }
     Gen.lift $ Bus.raise bus $ GameCreated newGame
     pure $ CallReply id $ s { knownGames = newGame : existingGames  }

findAll :: Effect (List RunningGame)
findAll =
  Gen.call serverName \s@{ knownGames } -> do
     pure $ CallReply knownGames s

findById :: String -> Effect (Maybe RunningGame)
findById id =
  Gen.call serverName \s@{ knownGames } -> do
     pure $ CallReply (head $ filter (\g -> g.id == id) knownGames) s

startLink :: StartArgs -> Effect StartLinkResult
startLink args =
   Gen.startLink serverName (init args)

init :: StartArgs -> Gen.Init State  Unit
init args = do
  pure $ { knownGames : nil }
