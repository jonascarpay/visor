{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BangPatterns #-}
module Batch where

import Game
import Dataset
import qualified Data.ByteString as BS
import Data.Serialize
import Data.Foldable
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

-- | A list of batches, where every element of the list corresponds to a
--   different feature and hence has a different shape
type BatchF = [Batch]

-- | If we have an image and the labels associated with that image, we
--   need a list of features to interpret both the image and the labels.
--   Labels need to be interpreted because they do not contain any information
--   about their cardinality themselves, that information is included in the
--   feature definition.
interpret :: (RGB, [Maybe Int]) -> [Feature] -> [Batch]
interpret (img, lbls) feats = zipWith Batch inputMatrices outputMatrices
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

mergeBatch :: Batch -> Batch -> Batch
mergeBatch (Batch x1 y1) (Batch x2 y2) = Batch (x1 === x2) (y1 === y2)

mergeBatchF :: [Batch] -> [Batch] -> [Batch]
mergeBatchF = zipWith mergeBatch

readMinibatch :: FilePath -> Game -> Dataset -> IO BatchF
readMinibatch f (Game _ feats _) (Dataset _ lblFn r w d) =
  do img <- loadImage f r w d
     return $ interpret (img, lblFn f) feats

batchFromSet :: Game -> Dataset -> IO BatchF
batchFromSet g d =
  do (f:fs) <- files d
     bsInit <- readMinibatch f g d
     foldlM comb bsInit fs
  where
    comb !b f = mergeBatchF b <$> readMinibatch f g d

genBatch :: Game -> Dataset -> IO ()
genBatch g@(Game _ feats _) d =
  do bs <- batchFromSet g d
     _ <- traverse (\(f, b) -> saveBatch b ("data"</>f)) $ zip (name<$>feats) bs
     return ()

saveBatch :: Batch -> FilePath -> IO ()
saveBatch (Batch im om) f = BS.writeFile f encoded
  where
    encoded = encode (toLists im, toLists om)
