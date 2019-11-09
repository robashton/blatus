module Pure.Game where


import Prelude

import Data.Foldable (foldl)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Pure.Math (Rect, Point)

newtype HtmlColor = HtmlColor String
derive instance ntHtmlColor :: Newtype HtmlColor _

newtype EntityId = EntityId String
derive instance ntEntityId :: Newtype EntityId _

type Renderable = { transform :: Rect
                  , color :: HtmlColor
                  , rotation :: Number
                  }

newtype EntityBehaviour = EntityBehaviour (Entity -> Entity)

type Entity = { id :: EntityId
              , location :: Point
              , rotation :: Number
              , renderables :: List Renderable
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
                   , rotation: 0.0
                   , behaviour : (EntityBehaviour rotate : Nil)
                   , renderables : ({transform: { x: (-12.5)
                                                , y: (-12.5)
                                                , width: 25.0
                                                , height: 25.0
                                                }
                                   , rotation: 0.0
                                   , color: HtmlColor "#f00"
                                   }) : Nil
                                 }

rotate :: Entity -> Entity
rotate e@{ rotation } =
  e { rotation = rotation + 0.01 }


tick :: Game -> Game 
tick game@ { entities } =
  game { entities = map (\e -> foldl (\i (EntityBehaviour f) -> f i) e e.behaviour) entities }
