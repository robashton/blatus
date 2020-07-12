module Pure.RunningGame where

import Prelude

import Erl.Process.Raw (Pid)
import Erl.Process ((!))
import Erl.Atom (atom)
import Erl.Data.Tuple (tuple2, tuple3)
import Data.Either (Either(..))
import Erl.Data.Binary (Binary(..))
import Data.Maybe (Maybe(..))
import Erl.Data.Map as Map
import Erl.Process (Process)
import Data.Traversable (traverse)
import Data.Newtype (wrap, unwrap)
import Effect (Effect)
import Erl.Data.List (List)
import Pinto as Pinto
import Pinto (ServerName(..), StartLinkResult)
import Pinto.Gen (CallResult(..), CastResult(..))
import Pinto.Gen as Gen
import Pinto.Timer as Timer
import Pinto.Monitor as Monitor
import Pure.Api (RunningGame)
import Pure.Logging as Log


type State = { game :: RunningGame }
data Msg = Tick

type StartArgs = { game :: RunningGame }

serverName :: String -> ServerName State Msg
serverName id = Local $ atom $ "runninggame-" <> id

startLink :: StartArgs -> Effect StartLinkResult
startLink args =
  Gen.buildStartLink (serverName args.game.id) (init args) $ Gen.defaultStartLink { handleInfo = handleInfo }

init :: StartArgs -> Gen.Init State Msg
init { game } = do
  Gen.lift $ Log.info Log.RunningGame "Started game" game
  self <- Gen.self
  Gen.lift do
    pure $ { game }

handleInfo :: Msg -> State -> Gen.HandleInfo State Msg
handleInfo msg state@{ game } = do
  case msg of
     Tick -> do
        self <- Gen.self
        Gen.lift do
          pure $ CastNoReply state 

