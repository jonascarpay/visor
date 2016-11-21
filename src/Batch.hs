module Batch where

import Numeric.LinearAlgebra

-- | A batch consists of a set of samples, and a
--   corresponding matrix of correct
--   classifications. A batch is used to train a
--   network and evaluate performance.
data Batch = Batch { input  :: Matrix R
                   , output :: Matrix R
                   }

-- | A dummy batch generator. Consists of k arms of n points each,
--   arranged in a spiral around (0,0).
spiral :: Int -> Int -> IO Batch
spiral n k = do t' <- (*0.01) . flatten <$> randn (n*k) 1
                let r = vjoin . replicate k $ linspace n (0.1,1)
                    t = t' + linspace (n*k) (0,fromIntegral k*4)
                    s = r * cmap sin t
                    c = r * cmap cos t
                    y = toIndexMatrix ([0..(k-1)] >>= replicate n) k
                return $ Batch (fromColumns [s,c]) y

-- Converts a list of r indices and an integer c to an
-- r x c matrix with 1 at horizontal position in the list
-- and 0 otherwise.
-- toIndexMatrix [3,2,1] 4 =
-- 0 0 0 1
-- 0 0 1 0
-- 0 1 0 0
toIndexMatrix :: [Int] -> Int -> Matrix R
toIndexMatrix y k = (length y><k) $ y >>= toIndex
 where toIndex y = replicate y 0 ++ [1] ++ replicate (k-y-1) 0

