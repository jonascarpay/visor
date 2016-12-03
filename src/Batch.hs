{-# LANGUAGE DeriveGeneric, DefaultSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Batch where

import Util()
import Numeric.LinearAlgebra
import Vision.Image
import Data.Serialize
import GHC.Generics (Generic(..))
import qualified Data.Vector.Storable as V
import qualified Vision.Image.Class as IC

-- | A NetBatch consists of a set of samples, and a
--   corresponding matrix of correct
--   classifications. They are used to train a
--   network and evaluate performance.
data NetBatch = NetBatch { input  :: Matrix R
                         , output :: Matrix R
                         } deriving Generic

instance Serialize NetBatch

stack :: NetBatch -> NetBatch -> NetBatch
stack (NetBatch i1 o1) (NetBatch i2 o2) = NetBatch (i1 === i2) (o1 === o2)

-- | A dummy batch generator. Consists of k arms of n points each,
--   arranged in a spiral around (0,0).
spiral :: Int -> Int -> IO NetBatch
spiral n k = do t' <- (*0.01) . flatten <$> randn (n*k) 1
                let r = vjoin . replicate k $ linspace n (0.1,1)
                    t = t' + linspace (n*k) (0,fromIntegral k*4)
                    s = r * cmap sin t
                    c = r * cmap cos t
                    y = toIndexMatrix' ([0..(k-1)] >>= replicate n) k
                return $ NetBatch (fromColumns [s,c]) y

-- | Converts an image in friday's format to a vector of
--   0-1 normalized values in hmatrix's vector format.
--   Even though I think both are the same Data.Vector
--   underneath.
imageToVector :: RGB -> Vector R
imageToVector = fromList . (>>= unpackPixel) . V.toList . IC.vector
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
