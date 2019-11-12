module Pure.Game where


import Prelude

import Data.Foldable (foldl)
import Data.List (List(..), elem, (:))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Math (cos, pi, sin) as Math
import Pure.Math (Rect, Point)
import Simple.JSON (class ReadForeign, class WriteForeign)

newtype HtmlColor = HtmlColor String
derive instance ntHtmlColor :: Newtype HtmlColor _

newtype EntityId = EntityId String
derive instance ntEntityId :: Newtype EntityId _
derive newtype instance eqEntityId :: Eq EntityId
derive newtype instance readEntityId :: ReadForeign EntityId
derive newtype instance writeEntityId :: WriteForeign EntityId
derive newtype instance showEntityId :: Show EntityId
derive newtype instance ordEntityId :: Ord EntityId

type Renderable = { transform :: Rect
                  , color :: HtmlColor
                  , rotation :: Number
                  }

data EntityCommand = PushForward | PushBackward | TurnLeft | TurnRight

derive instance eqEntityCommand :: Eq EntityCommand

newtype EntityBehaviour = EntityBehaviour (Entity -> Entity)

type Entity = { id :: EntityId
              , location :: Point
              , velocity :: Point
              , rotation :: Number
              , renderables :: List Renderable
              , commandHandlers :: List (EntityCommand -> Entity -> Entity)
              , behaviour :: List EntityBehaviour
              }


type Game =  {
  entities  :: List Entity
  }

initialModel :: Game
initialModel = {
  entities :  (tank (EntityId "player") { x: 20.0, y: 20.0 } ) : Nil
}

tank :: EntityId -> Point -> Entity
tank id location = { id
                   , location
                   , velocity: { x: 0.0, y: 0.0 }
                   , rotation: 0.0
                   , behaviour : (EntityBehaviour rotate : Nil)
                   , commandHandlers : Nil  
                   , renderables : ({transform: { x: (-12.5)
                                                , y: (-12.5)
                                                , width: 25.0
                                                , height: 25.0
                                                }
                                   , rotation: 0.0
                                   , color: HtmlColor "#f00"
                                   }) : Nil
                                 }


type DrivenConfig = { maxSpeed :: Number
                    , acceleration :: Number
                    , turningSpeed :: Number
                    }

--driven :: DrivenConfig -> Entity -> Entity
--driven config e =
--  if elem PushForward e.activeCommands then applyThrust config.acceleration config.maxSpeed e 
--     else e

applyThrust :: Number -> Number -> Entity -> Entity
applyThrust accel maxSpeed entity@{ velocity } =
  entity { velocity = { x: velocity.x + xvel, y: velocity.y + yvel } }
      where 
        angle = entity.rotation  * Math.pi * 2.0
        xvel = (Math.cos angle) * accel
        yvel = (Math.sin angle) * accel


rotate :: Entity -> Entity
rotate e@{ rotation } =
  e { rotation = rotation + 0.01 }


tick :: Game -> Game 
tick game@ { entities } =
  game { entities = map (\e -> foldl (\i (EntityBehaviour f) -> f i) e e.behaviour) entities }
