module Sisy.BuiltIn where

import Data.Maybe (Maybe)
import Data.Symbol (SProxy(..))
import Data.Variant (Variant, inj)
import Sisy.Math (Point)
import Sisy.Runtime.Entity (EntityId)

data Mass
  = Fixed Number
  | Infinite
  | NoMass

type Impact
  = { force :: Number, source :: EntityId }

impact :: forall r. Impact -> Variant ( impact :: Impact | r )
impact = inj (SProxy :: SProxy "impact")

type Damage
  = { amount :: Number, location :: Point, source :: Maybe EntityId }

damage :: forall r. Damage -> Variant ( damage :: Damage | r )
damage = inj (SProxy :: SProxy "damage")

type EntityDestroyed
  = { entity :: EntityId, destroyer :: Maybe EntityId }

entityDestroyed :: forall r. EntityDestroyed -> Variant ( entityDestroyed :: EntityDestroyed | r )
entityDestroyed = inj (SProxy :: SProxy "entityDestroyed")

type BulletFired
  = { owner :: EntityId, location :: Point, velocity :: Point, power :: Number }

bulletFired :: forall r. BulletFired -> Variant ( bulletFired :: BulletFired | r )
bulletFired ev = inj (SProxy :: SProxy "bulletFired") ev

type UpdateServerState
  = { location :: Point
    , velocity :: Point
    , rotation :: Number
    }

updateServerState :: forall r. UpdateServerState -> Variant ( updateServerState :: UpdateServerState | r )
updateServerState ev = inj (SProxy :: SProxy "updateServerState") ev
