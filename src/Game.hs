{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Game where

import Util
import Data.ByteString as BS hiding (putStrLn)
import Control.Monad.Trans.Resource

-- | A Game defines where to get a certain data set,
-- and what features to extract from it
data Game = Game { title    :: String
                 , features :: [Feature]
                 }

-- | A data set defines a set of samples for some game
data Dataset =
  Dataset
    { -- ^ Absolute paths to the images in the data set
      rootDir :: FilePath,
      -- ^ The labels to extract from an image. The order and length
      --   of the labels should be the same as the total number of
      --   feature positions for the given game. Paths are absolute.
      labels :: FilePath -> [Maybe Int],
      -- ^ The rectangle to crop the images to. This should be the
      --   largest area that still captures the game screen.
      cropRect :: Rect,
      -- ^ Indicates the number of extra pixels we can crop off
      --  in all directions. This is used to apply a random
      --  translation to the image.
      wiggle :: Int,
      -- ^ Whether or not to apply random color distortion to the
      --   sample images
      distort :: Bool
    }

-- | Loads an image and applies desired transformations
loadImage :: ByteString -- ^ ByteString of the image to load. We use a
                        --   ByteString representation because conduit
                        --   handles the IO
          -> Rect -- ^ Cropping rectangle
          -> Int -- ^ Indicates the number of extra pixels we can crop off
                 --  in all directions. This is used to apply a random
                 --  translation to the image.
          -> Bool -- ^ Wether or not to apply color distortions to the image
          -> ResIO RGBDelayed
loadImage bs (Rect x y w h) wig dis = undefined
