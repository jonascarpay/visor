{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
module Batch where

import Game
import Dataset
import qualified Data.ByteString as BS
import Data.Serialize
import Numeric.LinearAlgebra
import Vision.Image
import System.Directory
import System.FilePath
import qualified Data.Vector.Storable as V
import qualified Vision.Image.Class as IC

-- | A batch consists of a set of samples, and a
--   corresponding matrix of correct
--   classifications. A batch is used to train a
--   network and evaluate performance.
data Batch = Batch { input  :: Matrix R
                   , output :: Matrix R
                   }

-- | A sample consists of an input vecor and its
--   classification vector
data Sample = Sample { features :: Vector R
                     , classes  :: Vector R
                     }

-- | A dummy batch generator. Consists of k arms of n points each,
--   arranged in a spiral around (0,0).
spiral :: Int -> Int -> IO Batch
spiral n k = do t' <- (*0.01) . flatten <$> randn (n*k) 1
                let r = vjoin . replicate k $ linspace n (0.1,1)
                    t = t' + linspace (n*k) (0,fromIntegral k*4)
                    s = r * cmap sin t
                    c = r * cmap cos t
                    y = toIndexMatrix' ([0..(k-1)] >>= replicate n) k
                return $ Batch (fromColumns [s,c]) y

-- | Converts an image in friday's format to a vector of
--   0-1 normalized values in hmatrix's vector format.
--   Even though I think both are the same Data.Vector
--   underneath.
imageToSample :: RGB -> Vector R
imageToSample = fromList . (>>= unpackPixel) . V.toList . IC.vector
  where norm i = fromIntegral i / 255
        unpackPixel (RGBPixel r g b) = [norm r, norm g, norm b]

-- | Converts a list of r indices and an integer c to an
--   r x c matrix with 1 at horizontal position in the list
--   and 0 otherwise.
--   toIndexMatrix' [3,2,1] 4 =
--   0 0 0 1
--   0 0 1 0
--   0 1 0 0
toIndexMatrix :: [Maybe Int] -> Int -> Matrix R
toIndexMatrix y k = (length y><k) $ y >>= toIndex
  where toIndex (Just y) = replicate y 0 ++ [1] ++ replicate (k-y-1) 0
        toIndex Nothing  = replicate k 0

toIndexMatrix' :: [Int] -> Int -> Matrix R
toIndexMatrix' = toIndexMatrix . fmap Just

-- | A MiniBatch is a batch that only contains information for a single
--   feature of a single image
type MiniBatch = (Matrix R, Matrix R)

-- | MiniBatchF is a list of minibatches that each correspond to a different feature
type MiniBatchF = [MiniBatch]
type BatchF = [Batch]

-- | If we have an image and the labels associated with that image, we
--   need a list of features to interpret both the image and the labels.
--   Labels need to be interpreted because they do not contain any information
--   about their cardinality themselves, that information is included in the
--   feature definition.
interpret :: (RGB, [Maybe Int]) -> [Feature] -> [MiniBatch]
interpret (img, lbls) feats = zipWith (,) inputMatrices outputMatrices
  where

    extractions :: [[RGB]]
    extractions = fmap (extractFeature img) feats

    -- Take a flattened list of labels, group them by feature they
    -- correspond to, and turn labels for the same feature into a matrix
    group :: [Maybe Int] -> [Feature] -> [Matrix R]
    group lbls (Feature _ (length -> l) _ _ c:fs) =
      toIndexMatrix (take l lbls) c : group (drop l lbls) fs

    inputMatrices :: [Matrix R]
    inputMatrices = fmap (fromRows . fmap imageToSample) extractions

    outputMatrices :: [Matrix R]
    outputMatrices = group lbls feats

mergeBatches :: Batch -> Batch -> Batch
mergeBatches (Batch x1 y1) (Batch x2 y2) = Batch (x1 === x2) (y1 === y2)

-- | The outer lists corresponds to images, the inner list corresponds to
--   features. In the output, different images are collected and the elemens
--   of the list correspond to diffent features.
consolidate :: [[MiniBatch]] -> [Batch]
consolidate = foldr1 (zipWith mergeBatches) . (fmap.fmap) (uncurry Batch)

add :: MiniBatchF -> BatchF -> BatchF
add = zipWith (\(x1, y1) (Batch x2 y2) -> Batch (x2 === x1) (y2 === y1))

genBatches :: Game
           -> IO ()
genBatches (Game title feats sets) =
  do createDirectoryIfMissing True "data"
     setImgs <- loadFromSet (head sets)
     let batchify :: [Batch]
         batchify = consolidate . fmap (`interpret` feats) $ setImgs
         tagged :: [(String, Batch)]
         tagged = zipWith (\f b -> (name f, b)) feats batchify

     traverse (\(s,b) -> saveBatch b ("data"</>title ++ s)) tagged
     return ()

saveBatch :: Batch -> FilePath -> IO ()
saveBatch (Batch im om) f = BS.writeFile f encoded
  where
    encoded = encode (toLists im, toLists om)
