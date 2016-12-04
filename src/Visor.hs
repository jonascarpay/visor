{-# LANGUAGE DeriveGeneric #-}
module Visor where

import Network
import Game
import Batch
import Util
import Numeric.LinearAlgebra
import Vision.Image
import Vision.Primitive.Shape
import Vision.Primitive
import Data.Serialize
import GHC.Generics (Generic(..))

data Visor = Visor { visorName :: String
                   , nets :: [Network]
                   } deriving Generic

instance Serialize Visor

-- | A visor consists of multiple networks. A network is fed
--   batches. A VBatch is a collection of these batches
--   that can be fed to a visor.
type VBatch = [NetBatch]

-- | Generate an initial visor for a game.
fromGame :: Game -> IO Visor
fromGame game =
  do nets <- traverse forFeature (features game)
     return $ Visor (title game) nets
  where
    -- The network for a feature always has one output more than
    -- the feature cardinality. The extra output is used for
    -- the case where the output is undefined
    forFeature :: Feature -> IO Network
    forFeature (Feature _ _ _ (rw,rh) k) = initNet (rw*rh*3) (k+1) [100] 1 1

-- | For a given set of features, and a pair of an image and labels,
--   extract the features from the image and associate the labels
--   with those features.
toVBatch :: [Feature] -> LabeledImage -> VBatch
toVBatch fs (img, lbls) = zipWith NetBatch xs ys
  where
    xs :: [Matrix R]
    xs = extractFeature img <$> fs

    ys :: [Matrix R]
    ys = zipWith toIndexMatrix labelsGrouped (cardinality <$> fs)

    labelsGrouped = matchShape lbls (fmap positions fs)

    matchShape _   []     = []
    matchShape ins (e:es) = let (pre,post) = splitAt (length e) ins
                             in pre : matchShape post es

-- | Extracts a single feature from an image and returns it as
--   a matrix.
extractFeature :: RGBDelayed -> Feature -> Matrix R
extractFeature img (Feature _ pos (fw,fh) (rx, ry) _) = combine resized
  where
    (Z:.ih:.iw) = shape img

    -- TODO: document arguments
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
    crops :: [RGBDelayed]
    crops = fmap (`crop` img) cropAreas
    resized :: [RGBDelayed]
    resized = fmap (resize Bilinear $ ix2 ry rx) crops
    combine :: [RGBDelayed] -> Matrix R
    combine = fromRows . fmap (imageToVector . convert)

vTrain :: Visor -> VBatch -> ([Network], [Double])
vTrain (Visor _ nets) vb = unzip $ zipWith train nets vb

vAccuracy :: Visor -> VBatch -> [Double]
vAccuracy (Visor _ nets) vb = (*100) <$> zipWith accuracy nets vb

