{-# LANGUAGE ScopedTypeVariables #-}

module Dataset where

import Util
import Data.Word
import Vision.Primitive
import Vision.Image as I
import Vision.Image.Storage.DevIL
import System.Random

-- | A data set defines a set of samples for some game
data Dataset =
  Dataset
    { -- Paths to the images in the data set
      files :: IO [FilePath],
      -- | The labels to extract from an image. The order and length
      -- of the labels should be the same as the total number of
      -- feature positions for the given game.
      labels :: FilePath -> IO [Maybe Int],
      -- | The rectangle to crop the images to. This should be the
      --   largest area that still captures the game screen.
      cropRect :: Rect,
      -- | Indicates the number of extra pixels we can crop off
      --  in all directions. This is used to apply a random
      --  translation to the image.
      wiggle :: Int,
      -- | Whether or not to apply random color distortion to the
      --   sample images
      distort :: Bool
    }

-- | Loads an image and applies desired transformations
loadImage :: FilePath -- ^ Path to the image to load
          -> Rect -- ^ Cropping rectangle
          -> Int -- ^ Indicates the number of extra pixels we can crop off
                 --  in all directions. This is used to apply a random
                 --  translation to the image.
          -> Bool -- ^ Wether or not to apply color distortions to the image
          -> IO RGB
loadImage f (Rect x y w h) wig dis = do Right (img :: RGB) <- load Autodetect f
                                        dx <- randomRIO (0, wig `div` 2)
                                        dy <- randomRIO (0, wig `div` 2)
                                        dw <- randomRIO (0, wig `div` 2)
                                        dh <- randomRIO (0, wig `div` 2)
                                        dr <- randomRIO (0.9, 1.1 :: Double)
                                        dg <- randomRIO (0.9, 1.1 :: Double)
                                        db <- randomRIO (0.9, 1.1 :: Double)
                                        let (translated :: RGB) = crop (Rect (x+dx) (y+dy) (w-wig-dw) (h-wig-dh)) img
                                            tr, tg, tb :: Word8 -> Word8
                                            tr = scaleWord8 dr
                                            tg = scaleWord8 dg
                                            tb = scaleWord8 db
                                            (discolored :: RGB) = I.map (\(RGBPixel r g b) -> RGBPixel (tr r) (tg g) (tb b)) translated
                                        if dis then return discolored
                                               else return translated
