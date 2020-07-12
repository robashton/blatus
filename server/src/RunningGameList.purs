module Pure.RunningGameList where
 
import Prelude
import Effect (Effect)
import Erl.Atom (atom)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap, unwrap)
import Effect (Effect)
import Erl.Data.List (List)
import Pinto (ServerName(..), StartLinkResult)
import Pinto.Gen (CallResult(..))
import Pinto.Gen as Gen

create :: String -> String -> Boolean -> Effect String
create playerName gameName public = pure "what"

type StartArgs = {}
type State = {}

serverName :: ServerName State Unit
serverName = Local $ atom "running_game_list"

startLink :: StartArgs -> Effect StartLinkResult
startLink args =
   Gen.startLink serverName (init args)

init :: StartArgs -> Gen.Init State  Unit
init args = do
  pure $ {}
