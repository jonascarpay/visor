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

forward3 :: Monad m => Volume -> Layer3 -> m Volume
forward3 x (Conv w b) = w `corr` x >>= computeP . (+^ b)
forward3 x ReLU       = computeP $ R.map (max 0) x
forward3 x Pool       = pool x


forward2 :: Matrix -> Layer2 -> DMatrix
forward2 _ _ = undefined

{-# INLINE backward3 #-}
backward3 :: Monad m => Layer3 -> Volume -> Volume -> Volume -> Double -> Double -> m (Layer3, DVolume)

backward3 (Conv w b) x y dy λ δ =
  do w `fullConv` dy
     undefined

backward3 Pool       x y dy λ δ = return (Pool, undefined)
backward3 ReLU       x y dy λ δ = return (ReLU, R.zipWith (\x t -> if t > 0 then x else 0) y dy)

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
pool :: Monad m => Volume -> m Volume
pool v = computeP $ R.traverse v shFn maxReg
  where
    n = 2
    shFn (Z:.d:.h:.w) = Z:. d `div` n :. h `div` n :. w `div` n
    maxReg lkUp (b:.y:.x) = maximum [ lkUp (b:.y + dy:.x + dx) | dy <- [0..n-1], dx <- [0 .. n-1]]

{-# INLINE rotate #-}
rotate :: (Source r e, Shape tail) => Array r ((tail :. Int) :. Int) e -> Array D ((tail :. Int) :. Int) e
rotate arr = backpermute sh invert arr
  where
    sh@(_:.h:.w) = extent arr
    invert (b:.y:.x) = b:.h-y-1:.w-x-1

-- TODO: accept delayed representations, inline corr
corr :: Monad m => Weights -> Volume -> m Volume
corr krns img = if kd /= id
                   then error "Weight / image depth mismatch"
                   else computeP $ fromFunction sh' convF
  where
    Z:.ki:.kd:.kh:.kw = extent krns
    Z:.    id:.ih:.iw = extent img
    sh' = Z:.ki:.ih-kh+1:.iw-kw+1

    {-# INLINE convF #-}
    convF :: DIM3 -> Double
    convF (Z:.od:.oh:.ow) = sumAllS $ krn *^ img'
      where
        krn  = slice krns (Z:.od:.All:.All:.All)
        img' = extract (Z:.0:.oh:.ow) (Z:.id:.kh:.kw) img

fullConv :: Monad m => Weights -> Volume -> m Volume
fullConv krn img = do krn' <- computeP $ rotate krn
                      img' <- computeP $ zeropad padSize img
                      krn' `corr` img'
  where
    _:.kh:.kw = extent krn
    padSize = if kh == kw then kh-1 else error "Non-square kernel"

{-# INLINE zeropad #-}
zeropad :: (Source r Double, Shape tail) => Int -> Array r ((tail :. Int) :. Int) Double -> Array D ((tail :. Int) :. Int) Double
zeropad n a = R.traverse a shFn padFn
  where
    _:.h:.w = extent a

    {-# INLINE shFn #-}
    shFn (b:.h:.w) = b:.h+2*n:.w+2*n
    {-# INLINE padFn #-}
    padFn lkFn (b:.y:.x)
      | y < n || y >= h + n || x < n || x >= w + n = 0
      | otherwise = lkFn (b:.y-n:.x-n)

img :: Volume
img  = fromListUnboxed (ix3 1 3 3) [1,2,3,1,0,1,0,1,1]

mask :: Weights
mask = fromListUnboxed (ix4 1 1 2 2) [1,0,0,0]

ic :: Monad m => m Volume
ic = do mask `fullConv` img
