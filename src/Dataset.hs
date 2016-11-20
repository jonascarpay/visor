module Dataset where

-- | A data set defines a set of samples for some game
data Dataset =
  Dataset
    { -- Paths to the images in the data set
      files :: IO [FilePath],
      -- | The labels to extract from an image. The order and length
      -- of the labels should be the same as the total number of
      -- feature positions for the given game.
      labels :: FilePath -> IO [Label],
      -- | A function to be applied to every image before using it
      -- for training. The main example is cropping.
      preprocess :: ImageFunction,
      -- | Indicates whether to apply random distortions to the image.
      -- Random distortions can make training more robust
      distort :: Bool
    }

data ImageFunction
type Label = Int
