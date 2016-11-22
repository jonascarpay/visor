module Game where

import Dataset
import Vision.Image
import Vision.Primitive.Shape
import Numeric.LinearAlgebra

-- | A Game defines where to get a certain data set,
-- and what features to extract from it
data Game = Game { title    :: String
                 , features :: [Feature]
                 , datasets :: [Dataset]
                 }

-- | A feature represents some metric to be obtained
-- from an image, and the positions it occurs in.
data Feature =
  Feature
    { -- | Name for this feature
      name :: String,
      -- | The center of the feature in the image.
      -- Values given should be between 0 and 1
      positions :: [(Double, Double)],
      -- | The area to be scanned for the feature.
      -- Values should be between 0 and 1
      dimensions :: (Double, Double),
      -- | The resolution to be used for this feature.
      -- Higher values mean more downsampling can be
      -- done during preprocessing, which leads to
      -- better performance
      resolution :: (Int, Int)
    }

extractFeature :: RGB -> Feature -> [Vector R]
extractFeature img (Feature _ pos (fw,fh) (rx, ry)) = undefined
