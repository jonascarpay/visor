module Game where

import Dataset
import Vision.Image
import Vision.Primitive.Shape
import Vision.Primitive
import Vision.Image.Storage.DevIL

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

-- | Extracts samples for some feature from an image
extractFeature :: Feature -> RGB -> [RGB]
extractFeature (Feature _ pos (fw,fh) (rx, ry)) img = resized
  where
    (Z:.ih:.iw) = shape img

    toArea :: Double -> Double -> Double -> Double -> Int -> Int -> Rect
    toArea cx cy fw fh iw ih = let xRel = cx - fw / 2
                                   yRel = cy - fh / 2
                                   wRel = fw
                                   hRel = fh
                                   x = round $ fromIntegral iw * xRel
                                   y = round $ fromIntegral ih * yRel
                                   w = round $ fromIntegral iw * wRel
                                   h = round $ fromIntegral ih * hRel
                                in Rect x y w h

    cropAreas :: [Rect]
    cropAreas = fmap (\(cx, cy) -> toArea cx cy fw fh iw ih) pos
    crops :: [RGB]
    crops = fmap (`crop` img) cropAreas
    resized :: [RGB]
    resized = fmap (resize Bilinear $ ix2 ry rx) crops

testFile :: FilePath
testFile = "/Users/jmc/Desktop/testout.png"

test :: Game -> IO (Maybe StorageError)
test (Game _ (feat:_) (set:_)) =
  do let (Dataset fs _ cr wig dist) = set
     f <- head <$> fs
     img <- loadImage f cr wig dist
     let feats = feat `extractFeature` img
         fnames = [ "/Users/jmc/Desktop/test" ++ show n ++ ".png" | n <- [(1::Int)..] ]
         saveIOs = zipWith (save PNG) fnames feats
     sequence_ saveIOs
     save PNG testFile img
