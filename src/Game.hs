{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Game where

import Util
import Label
import Codec.Picture
import Codec.Picture.Extra
import Conduit
import System.Random

-- | A Game defines where to get a certain data set,
-- and what features to extract from it
data Game = Game { title   :: String
                 , widgets :: [Widget]
                 }

-- | A widget represents a (possibly repeating) rectangular area of
--   the screen containing information to be extracted.
data Widget =
  Widget
    { -- ^ The resolution at which this is fed to the network. Higher means
      --   slower convergence, easier overfitting, and worse performance.
      --   Usually, we want the lowest value at which features are still clearly
      --   distinguishable. Because the convolutional network only accepts square
      --   inputs, the resolution is the same for the x and y axes.
      resolution :: Int,
      -- ^ The x, y positions at which this widget occurs on the screen. Values
      --   should be relative between 0 and 1. The coordinate corresponds to
      --   the location of the top left corner.
      position :: [(Double, Double)],
      -- ^ The dimensions of the widget, again, both between 0 and 1.
      dimensions :: (Double, Double),
      -- ^ The length of this list equals the amount of features to extract from
      --   a widget, and the values indicate how many possible values that feature
      --   takes. A digit, for example, would have a cardinality of 10. Note that
      --   Each feature is hardcoded to have an additional 'undefined/indeterminate'
      --   output, that is used in case the feature does not occur.
      cardinalities :: [Int]
    }

type WidgetLabel = [Label]

-- | A data set defines a set of samples for some game
data Dataset =
  Dataset
    { -- ^ Absolute paths to the images in the data set
      rootDir :: FilePath,
      -- ^ The labels to extract from an image. The order and length
      --   of the labels should be the same as the total number of
      --   feature positions for the given game. Paths are absolute.
      labels :: FilePath -> [WidgetLabel],
      -- ^ The rectangle to crop the images to. This should be the
      --   largest possible area that only captures the game screen.
      cropRect :: Rect Int,
      -- ^ Indicates the number of extra pixels we can crop off
      --  in all directions. This is used to apply a random
      --  translation to the image.
      wiggle :: Int,
      -- ^ Whether or not to apply random color distortion to the
      --   sample images
      distort :: Bool
    }

-- | Loads an image and applies desired transformations
datasetSource :: Dataset
              -> IOSrc (Palette, [WidgetLabel])
datasetSource (Dataset root lblFn (Rect x y w h) wig dist) =
  sourceDirectoryDeep True root .| loadImageC
  where
    loadImageC :: IOConduit FilePath (Palette, [WidgetLabel])
    loadImageC = awaitForever$
      \fp -> do o <- liftIO $ loadImage fp
                yield o

    loadImage :: FilePath -> IO (Palette, [WidgetLabel])
    loadImage fp = do Right img' <- readImage fp
                      dx <- randomRIO (0, wig)
                      dy <- randomRIO (0, wig)
                      dw <- randomRIO (0, wig)
                      dh <- randomRIO (0, wig)
                      dr :: Double <- randomRIO (0.9, 1.1)
                      dg :: Double <- randomRIO (0.9, 1.1)
                      db :: Double <- randomRIO (0.9, 1.1)
                      let img = convertRGB8 img'
                          imgCropped = crop (x+dx) (y+dy) (w-dx-dw) (h-dy-dh) img
                          scaleMax x c = round $ min 255 $ fromIntegral x * c
                          distortColor (PixelRGB8 r g b) = PixelRGB8 (scaleMax r dr) (scaleMax g dg) (scaleMax b db)
                          distorted = pixelMap distortColor imgCropped

                      return (if dist then distorted else imgCropped, lblFn fp)

datasetSink :: IOSink (Palette, [WidgetLabel])
