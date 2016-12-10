{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.List.Split
import Data.Array.Repa hiding ((++))
import Data.Array.Repa.Algorithms.Convolve

type Weights = Array U DIM4 Double
type Vol     = Array U DIM3 Double
type DVol    = Array D DIM3 Double

conv :: Vol -> Weights -> DVol
conv img krns = if kd /= id
                   then error "Weight / image depth mismatch"
                   else fromFunction sh' convF
  where
    Z:.ki:.kd:.kh:.kw = extent krns
    Z:.id:.ih:.iw     = extent img
    rh = (kh - 1) `div` 2
    rw = (kw - 1) `div` 2
    sh' = Z:.ki:.ih-kh+1:.iw-kw+1
    convF :: DIM3 -> Double
    convF (Z:.od:.oh:.ow) = sumAllS $ krn *^ img'
      where
        krn  = slice krns (Z:.od:.All:.All:.All)
        img' = extract (Z:.0:.oh:.ow) (Z:.id:.kh:.kw) img

img :: Vol
img = fromListUnboxed (ix3 3 4 4) $ replicate 16 1 ++ replicate 16 2 ++ replicate 16 3

mask :: Weights
mask = fromListUnboxed (ix4 1 1 2 2) $ replicate (1*2*2) 1

main :: IO ()
main = do (c :: Vol) <- computeP $ img `conv` mask
          print c
