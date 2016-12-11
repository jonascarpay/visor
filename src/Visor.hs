{-# LANGUAGE DeriveGeneric #-}
module Visor where

import Network
import Game
import Batch
import Util
import Numeric.LinearAlgebra
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
    forFeature (Feature _ _ _ (rw,rh) k (NetConfig λ δ hs)) =
      initNet (rw*rh*3) (k+1) hs λ δ

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

-- | Extracts a single feature from an image and returns all occurrences
--   of that feature as both a matrix and a list of images
extractFeature' :: RGBDelayed -> Feature -> (Matrix R, [RGBDelayed])
extractFeature' = undefined -- (combine resized, resized)

extractFeature :: RGBDelayed -> Feature -> Matrix R
extractFeature img f = fst $ extractFeature' img f

vTrain :: Visor -> VBatch -> ([Network], [Double])
vTrain (Visor _ nets) vb = unzip $ zipWith train nets vb

vFeed :: Game -> Visor -> RGBDelayed -> [Maybe Int]
vFeed (Game _ fs) (Visor _ nets) img = max
  where
    xs = fmap (extractFeature img) fs
    ys = zipWith feed nets xs
    vecs = ys >>= toRows
    max = fmap (min1 . maxIndex) vecs
    min1 x
      | x < 1 = Nothing
      | otherwise = Just (x-1)

vAccuracy :: Visor -> VBatch -> [Double]
vAccuracy (Visor _ nets) vb = (*100) <$> zipWith accuracy nets vb
