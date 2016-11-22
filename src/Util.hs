module Util where

import Numeric.LinearAlgebra

colSums, rowSums :: Matrix R -> Vector R
rowSums m = m #> konst 1 (cols m)
colSums m = konst 1 (rows m) <# m

normalizeRows :: Matrix R -> Matrix R
normalizeRows m = m / asColumn (rowSums m)

-- | Gives the average of the sum of the rows of
--   some matrix. This is mostly used to take the
--   average output of a matrix where there is only
--   one non-zero value in every row
avgRowSum :: Matrix R -> Double
avgRowSum m = sumElements m / fromIntegral (rows m)

merge :: [a] -> [a] -> [a]
merge [] ys     = ys
merge (x:xs) ys = x:merge ys xs
