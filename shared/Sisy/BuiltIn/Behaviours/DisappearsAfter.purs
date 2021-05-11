module Sisy.BuiltIn.Behaviours.DisappearsAfter where

import Data.Exists (Exists, mkExists)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Variant (Variant, default, inj, onMatch)
import Prelude (bind, discard, pure, ($), (-), (<=))
import Sisy.BuiltIn (EntityDestroyed, entityDestroyed)
import Sisy.Runtime.Behaviour as B
import Sisy.Runtime.Entity (EntityBehaviour(..), EntityId)

init ::
  forall entity cmd ev.
  Int ->
  Exists (EntityBehaviour cmd (Event ev) entity)
init after =
  mkExists
    $ EntityBehaviour
        { state: { ticks: after }
        , handleCommand:
            \command s ->
              onMatch
                { tick:
                    \_ -> do
                      id <- B.id
                      if s.ticks <= 0 then do
                        B.raiseEvent (entityDestroyed { entity: id, destroyer: Just id })
                        pure s
                      else
                        pure $ s { ticks = s.ticks - 1 }
                }
                (default (pure s))
                command
        }

type Event r
  = ( entityDestroyed :: EntityDestroyed
    | r
    )
