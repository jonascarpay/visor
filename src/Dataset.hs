module Dataset where

import Vision.Primitive

-- | A data set defines a set of samples for some game
data Dataset =
  Dataset
    { -- Paths to the images in the data set
      files :: IO [FilePath],
      -- | The labels to extract from an image. The order and length
      -- of the labels should be the same as the total number of
      -- feature positions for the given game.
      labels :: FilePath -> IO [Maybe Int],
      -- | The rectangle to crop the images to.
      crop :: Rect,
      -- | Maximum random offset to be applied to the crop window.
      --   This is used to randomly distort the image. The unit
      --   is pixels.
      wiggle :: Int,
      -- | Whether or not to apply random color distortion to the
      --   sample images
      distort :: Bool
    }

