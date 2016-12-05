{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Game where

import Util
import Data.ByteString as BS hiding (putStrLn)
import Data.Word
import Conduit
import Control.Monad
import Control.Monad.Trans.Resource
import Vision.Primitive
import Vision.Image as I
import Vision.Image.Storage.DevIL
import System.Random


-- | A Game defines where to get a certain data set,
-- and what features to extract from it
data Game = Game { title    :: String
                 , features :: [Feature]
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
      --   Lower values mean more downsampling can be
      --   done during preprocessing, which leads to
      --   better performance
      resolution :: (Int, Int),
      -- | The number of different values this feature can take
      cardinality :: Int
    }

-- | A data set defines a set of samples for some game
data Dataset =
  Dataset
    { -- Absolute paths to the images in the data set
      rootDir :: FilePath,
      -- | The labels to extract from an image. The order and length
      --   of the labels should be the same as the total number of
      --   feature positions for the given game. Paths are absolute.
      labels :: FilePath -> [Maybe Int],
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
loadImage :: ByteString -- ^ ByteString of the image to load. We use a
                        --   ByteString representation because conduit
                        --   handles the IO
          -> Rect -- ^ Cropping rectangle
          -> Int -- ^ Indicates the number of extra pixels we can crop off
                 --  in all directions. This is used to apply a random
                 --  translation to the image.
          -> Bool -- ^ Wether or not to apply color distortions to the image
          -> ResIO RGBDelayed
loadImage bs (Rect x y w h) wig dis =
  do [dx, dy, dw, dh] <- replicateM 4 (liftIO $ randomRIO (0, wig `div` 2))
     [dr, dg, db]     <- replicateM 3 (liftIO $ randomRIO (0.9, 1.1 :: Double))
     let eimg :: Either StorageError RGBDelayed = loadBS Autodetect bs
         img = case eimg of
                 Right x -> x
                 Left err -> error $ show err
         (translated :: RGBDelayed) = crop (Rect (x+dx) (y+dy) (w-wig-dw) (h-wig-dh)) img
         tr, tg, tb :: Word8 -> Word8
         tr = scaleWord8 dr
         tg = scaleWord8 dg
         tb = scaleWord8 db
         (discolored :: RGBDelayed) = I.map (\(RGBPixel r g b) -> RGBPixel (tr r) (tg g) (tb b)) translated
     if dis then return discolored
            else return translated
