module Blatus.Types where

import Prelude
import Blatus.GenericJSON (writeTaggedSumRep, taggedSumRep)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)
import Data.Symbol (SProxy(..))
import Data.Variant (Variant, inj)
import Simple.JSON (class ReadForeign, class WriteForeign)
import Sisy.BuiltIn.Behaviours.BasicBitchPhysics (Mass)
import Sisy.BuiltIn.Behaviours.Damageable (EntityDestroyed)
import Sisy.BuiltIn.Behaviours.FiresBullets (BulletFired)
import Sisy.BuiltIn.Extensions.Bullets as Bullets
import Sisy.BuiltIn.Extensions.Collider (CollisionInfo)
import Sisy.Math (Point, Rect)
import Sisy.Runtime.Entity (EntityId)
import Sisy.Types (Empty)

type RegisteredPlayer
  = { id :: EntityId
    , lastTick :: Int
    , score :: Int
    }

type CollectableArgs
  = { width :: Number, height :: Number, collectableType :: CollectableType }

data EntityClass
  = Tank
  | Asteroid { width :: Number, height :: Number }
  | Collectable CollectableArgs

data CollectableType
  = Rock Int

type GameEntity
  = ( networkSync :: Boolean
    , class :: EntityClass
    , health :: Number
    , shield :: Number
    , velocity :: Point
    , mass :: Mass
    , friction :: Number
    , aabb :: Rect
    )

derive instance genericEntityClass :: Generic EntityClass _

instance showEntityClass :: Show EntityClass where
  show = genericShow

derive instance eqEntityClass :: Eq EntityClass

instance writeForeignEntityClass :: WriteForeign EntityClass where
  writeImpl = writeTaggedSumRep

instance readForeignEntityClass :: ReadForeign EntityClass where
  readImpl = taggedSumRep

derive instance genericCollectableType :: Generic CollectableType _

instance showCollectableType :: Show CollectableType where
  show = genericShow

derive instance eqCollectableType :: Eq CollectableType

instance writeForeignCollectableType :: WriteForeign CollectableType where
  writeImpl = writeTaggedSumRep

instance readForeignCollectableType :: ReadForeign CollectableType where
  readImpl = taggedSumRep

type EntityCommand
  = ( damage :: { amount :: Number, source :: Maybe EntityId }
    , pushForward :: Empty
    , pushBackward :: Empty
    , turnLeft :: Empty
    , turnRight :: Empty
    , startFireBullet :: Empty
    , stopFireBullet :: Empty
    , stopPushForward :: Empty
    , stopPushBackward :: Empty
    , stopTurnLeft :: Empty
    , stopTurnRight :: Empty
    , updateServerState ::
        { location :: Point
        , velocity :: Point
        , rotation :: Number
        }
    )

type GameEvent
  = ( bulletFired :: BulletFired
    , entityCollided :: CollisionInfo
    , bulletHit :: Bullets.BulletHit
    , entityDestroyed :: EntityDestroyed
    , playerSpawn :: PlayerSpawn
    )

type PlayerSpawn
  = { id :: EntityId, x :: Number, y :: Number }

playerSpawn :: PlayerSpawn -> Variant GameEvent
playerSpawn = inj (SProxy :: SProxy "playerSpawn")
