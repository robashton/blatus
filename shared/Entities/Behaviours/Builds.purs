module Blatus.Entities.Behaviours.Builds where

import Prelude

import Blatus.Types (Build, BuildRequested, buildRequested)
import Data.Exists (Exists, mkExists)
import Data.Variant (default, onMatch)
import Sisy.Runtime.Behaviour as B
import Sisy.Runtime.Entity (EntityBehaviour(..), EntityId(..))

init ::
  forall entity cmd ev.
  Exists (EntityBehaviour (Command cmd) (Event ev) (State entity))
init =
  mkExists
    $ EntityBehaviour
        { state: { count: 0 }
        , handleCommand:
            \command s ->
              onMatch
                { build:
                    \cmd -> do
                      -- TODO: throttle, etc
                      id <- B.id
                      B.raiseEvent
                        $ buildRequested
                            { id: id <> EntityId (show s.count)
                            , location: cmd.location
                            , entity: id
                            , template: cmd.template
                            }
                      pure $ s { count = s.count + 1 }
                }
                (default (pure s))
                command
        }

type State :: forall k. k -> k
type State r
  = ( | r )

type Command r
  = ( build :: Build
    | r
    )

type Event r
  = ( buildRequested :: BuildRequested
    | r
    )
