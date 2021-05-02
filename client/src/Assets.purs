module Assets where

import Prelude
import Data.Either (Either(..))
import Data.List (List(..), (:))
import Data.Map (Map)
import Data.Map (fromFoldable) as Map
import Data.Maybe (maybe)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff, error, makeAff)
import Graphics.Canvas (CanvasImageSource)
import Graphics.Canvas (tryLoadImage) as Canvas

type AssetPackage
  = Map String CanvasImageSource

loadImage :: String -> Aff CanvasImageSource
loadImage path = makeAff wrapped
  where
  wrapped cb = do
    _ <-
      Canvas.tryLoadImage path
        ( \maybeImage ->
            cb $ maybe (Left $ error $ "Could not load " <> path) Right maybeImage
        )
    pure mempty

load :: Aff AssetPackage
load = do
  ship <- loadImage "/art/ship.png"
  shield <- loadImage "/art/shield.png"
  pure $ Map.fromFoldable $ (Tuple "ship" ship) : (Tuple "shield" shield) : Nil
