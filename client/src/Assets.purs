module Assets where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe)
import Effect.Aff (Aff, error, makeAff)
import Graphics.Canvas (CanvasImageSource, tryLoadImage)
import Graphics.Canvas (tryLoadImage) as Canvas

type AssetPackage = {
  ship :: CanvasImageSource
  }


loadImage :: String ->  Aff CanvasImageSource
loadImage path = makeAff wrapped
  where wrapped cb = do
          _ <- Canvas.tryLoadImage path (\maybeImage ->
                  cb $ maybe (Left $ error $ "Could not load " <> path) Right maybeImage
                  )
          pure mempty 

load :: Aff AssetPackage
load = do
  ship <- loadImage "art/ship.png"
  pure $ {
    ship
  }
