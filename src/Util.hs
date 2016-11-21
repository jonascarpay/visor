module Util where

import Numeric.LinearAlgebra

colSums, rowSums :: Matrix R -> Vector R
rowSums m = m #> konst 1 (cols m)
colSums m = konst 1 (rows m) <# m

normalizeRows :: Matrix R -> Matrix R
normalizeRows m = m / asColumn (rowSums m)
