{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Volume where

import Data.Array.Repa as R hiding ((++))

data Layer3 = Conv Weights Bias
            | ReLU
            | Pool

data Layer2 = FC Matrix
            | SoftMax

forward3 :: Volume -> Layer3 -> DVolume
forward3 x (Conv w b) = (delay x `corr` w) +^ b
forward3 x ReLU = R.map (max 0) x
forward3 x Pool = pool x

forward2 :: Matrix -> Layer2 -> DMatrix
forward2 _ _ = undefined

{-# INLINE backward3 #-}
backward3 :: Layer3 -> Volume -> Volume -> (Layer3, DVolume)
backward3 (Conv w b) y dy = undefined
backward3 Pool       y dy = (Pool, undefined)
backward3 ReLU       y dy = (ReLU, R.zipWith (\x t -> if t > 0 then x else 0) y dy)

type Weights = Array U DIM4 Double
type Volume  = Array U DIM3 Double
type Matrix  = Array U DIM2 Double
type Vector  = Array U DIM1 Double
type Bias    = Volume
type DWeights = Array D DIM4 Double
type DVolume  = Array D DIM3 Double
type DMatrix  = Array D DIM2 Double
type DVector  = Array D DIM1 Double

{-# INLINE pool #-}
pool :: Volume -> DVolume
pool v = R.traverse v shFn maxReg
  where
    n = 2
    shFn (Z:.d:.h:.w) = Z:. d `div` n :. h `div` n :. w `div` n
    maxReg lkUp (b:.y:.x) = maximum [ lkUp (b:.y + dy:.x + dx) | dy <- [0..n-1], dx <- [0 .. n-1]]

-- TODO: kernels draaien ipv
{-# INLINE conv #-}
conv :: DVolume -> Weights -> DVolume
conv img krns = rotated `corr` krns
  where
    b:.h:.w = extent img
    invert (b:.y:.x) = b:.h-1-y:.w-1-x
    rotated = backpermute (b:.h:.w) invert img

{-# INLINE corr #-}
corr :: DVolume -> Weights -> DVolume
corr img krns = if kd /= id
                   then error "Weight / image depth mismatch"
                   else fromFunction sh' convF
  where
    imgPadded = zeropad 1 img
    Z:.ki:.kd:.kh:.kw = extent krns
    Z:.id:.ih:.iw     = extent imgPadded
    sh' = Z:.ki:.ih-kh+1:.iw-kw+1
    convF :: DIM3 -> Double
    convF (Z:.od:.oh:.ow) = sumAllS $ krn *^ img'
      where
        krn  = slice krns (Z:.od:.All:.All:.All)
        img' = extract (Z:.0:.oh:.ow) (Z:.id:.kh:.kw) imgPadded

{-# INLINE zeropad #-}
zeropad :: (Source r Double, Shape tail) => Int -> Array r ((tail :. Int) :. Int) Double -> Array D ((tail :. Int) :. Int) Double
zeropad n a = R.traverse a shFn padFn
  where
    _:.h:.w = extent a
    shFn (b:.h:.w) = b:.h+2*n:.w+2*n
    padFn lkFn (b:.y:.x)
      | y == 0 || y == h+1 || x == 0 || x == w+1 = 0
      | otherwise = lkFn (b:.y-1:.x-1)

img :: Volume
img = fromListUnboxed (ix3 1 1 6) [0,3,2,1,0,0]

mask :: Weights
mask = fromListUnboxed (ix4 1 1 2 3) [-1,1,-1, 1, -1, 1]
