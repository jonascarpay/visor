{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module VolumeSpec where

import Prelude hiding (map, zipWith)
import Test.QuickCheck
import Data.Array.Repa
import Data.Functor.Identity
import Volume

newtype VecA = VecA Vector deriving Show
instance Arbitrary VecA where
  arbitrary = do Positive (Small w) <- arbitrary
                 elems <- vector w
                 return . VecA $ fromListUnboxed (Z:.w) elems

newtype MatA = MatA Matrix deriving Show
instance Arbitrary MatA where
  arbitrary = do Positive (Small w) <- arbitrary
                 Positive (Small h) <- arbitrary
                 elems <- vector (w*h)
                 return . MatA $ fromListUnboxed (Z:.h:.w) elems

newtype VolA = VolA Volume deriving Show
instance Arbitrary VolA where
  arbitrary = do Positive (Small w) <- arbitrary
                 Positive (Small h) <- arbitrary
                 Positive (Small d) <- arbitrary
                 elems <- vector (w*h*d)
                 return . VolA $ fromListUnboxed (Z:.d:.h:.w) elems

newtype WgtA = WgtA Weights deriving Show
instance Arbitrary WgtA where
  arbitrary = do Positive (Small w) <- arbitrary
                 Positive (Small h) <- arbitrary
                 Positive (Small d) <- arbitrary
                 Positive (Small n) <- arbitrary
                 elems <- vector (w*h*d*n)
                 return . WgtA $ fromListUnboxed (Z:.n:.d:.h:.w) elems

newtype Layer3A = Layer3A Layer3 deriving Show
instance Arbitrary Layer3A where
  arbitrary = do Positive (Small w)  <- arbitrary
                 Positive (Small h)  <- arbitrary
                 Positive (Small d)  <- arbitrary
                 Positive (Small n)  <- arbitrary
                 Positive (Small ow) <- arbitrary
                 Positive (Small oh) <- arbitrary
                 seed <- arbitrary
                 o <- elements [ Pool, ReLU, randomConvLayer w h d n (w+ow) (h+oh) seed ]
                 return . Layer3A $ o

a `divs` b = b `mod` a == 0
a `approx` b = abs (a-b) < 1e-3
arr1 `approxA` arr2 =
  extent arr1 == extent arr2 &&
    and [ linearIndex arr1 i `approx` linearIndex arr2 i | i <- [0..size (extent arr1) -1]]

-- rotate
prop_rotateInvolutionM (MatA a) = once$ computeS (rotate . rotate $ a) == a
prop_rotateInvolutionV (VolA a) = once$ computeS (rotate . rotate $ a) == a
prop_rotateInvolutionW (WgtA a) = once$ computeS (rotate . rotate $ a) == a

prop_rotateSizeM (MatA a) = once$ w == rw && h == rh
  where r = computeS $ rotate a :: Matrix
        Z:.h:.w = extent a
        Z:.rh:.rw = extent r

prop_rotateSizeV (VolA a) = once$ w == rw && h == rh && d == rd
  where r = computeS $ rotate a :: Volume
        Z:.d:.h:.w = extent a
        Z:.rd:.rh:.rw = extent r

prop_rotateInv (MatA a) (Positive (Small y)) (Positive (Small x)) = once$
  y < h && x < w ==> a ! (Z:.y:.x) == a' ! (Z:.h-y-1:.w-x-1)
  where
    Z:.h:.w = extent a
    a' = computeS $ rotate a :: Matrix

-- pool
prop_poolSum (VolA a) = once$
  2 `divs` w && 2 `divs` h
  ==> max (maxElem a) 0 == maxElem a'
  where
    maxElem = foldAllS max 0
    a' = runIdentity $ pool a
    Z:._:.h:.w = extent a

prop_poolBackpropSumInvariant (VolA a) = once$
  h>2 && w>2 && h `mod` 2 == 0 && w `mod` 2 == 0 ==> runIdentity $
    do p  :: Volume <- pool a
       dp :: Volume <- computeP $ map (1/) p
       da <- poolBackprop a p dp
       sdp <- sumAllP dp
       sda <- sumAllP da
       return $ sda `approx` sdp
  where
    Z:._:.h:.w = extent a

-- relu
prop_reluSum (VolA a) = once . runIdentity $
  do f  <- forward3 a ReLU
     sf <- sumAllP f
     sa <- sumAllP a
     return $ sf >= sf

-- zeropad
prop_zeroPadAssoc (MatA a) (Positive (Small n1)) (Positive (Small n2)) = once$
    (computeS . zeropad n1 $ (computeS . zeropad n2 $ a :: Matrix) :: Matrix)
     == (computeS . zeropad (n1+n2)) a

prop_zeroPadSumInvariant (MatA a) = sumAllS (computeS (zeropad 1 a) :: Matrix) == sumAllS a

-- vvmult
prop_vecMult (VecA a) (VecA b) = abs (sumAllS a * sumAllS b - sumAllS (a `vvmult` b)) < 1e-3

-- corr
prop_corrExtent (WgtA a) (VolA b) = once$
    id == kd && ih >= kh && iw >= kw
    ==> shOut == (Z:.kn:.ih-kh+1:.iw-kw+1)
  where
    shOut = extent out
    Z:.id:.ih:.iw     = extent b
    Z:.kn:.kd:.kh:.kw = extent a
    out = runIdentity $ a `corr` b

-- Note that we could drop the d==1 constraint if the output of corr were four-dimensional
prop_corrIdentity (VolA a) = once$ d == 1 ==> ca == a
  where
    Z:.d:._:._ = extent a
    dirac = fromListUnboxed (Z:.1:.1:.1:.1) [1]
    ca = runIdentity $ dirac `corr` a

prop_scalarAssocCorr (WgtA a) (VolA b) (c :: Double) = once$
    id == kd && ih >= kh && iw >= kw
      ==> runIdentity $
        do ca   <- computeP $ map (*c) a
           ab   <- a `corr` b
           c_ab :: Volume <- computeP $ map (*c) ab
           ca_b <- ca `corr` b
           return $ ca_b `approxA` c_ab
  where
    Z:.id:.ih:.iw    = extent b
    Z:._:.kd:.kh:.kw = extent a

-- fullConv
prop_fullConvIdty (VolA a) = once$ d == 1 ==> ca == a
  where
    Z:.d:._:._ = extent a
    dirac = fromListUnboxed (Z:.1:.1:.1:.1) [1]
    ca = runIdentity $ dirac `fullConv` a

-- corrVolumes
prop_scalarAssocCorrVolumes (VolA a) (VolA b) (c :: Double) = once$
    id == kd && ih >= kh && iw >= kw
      ==> runIdentity $
        do ca   <- computeP $ map (*c) a
           ab   <- a `corrVolumes` b
           c_ab :: Weights <- computeP $ map (*c) ab
           ca_b <- ca `corrVolumes` b
           return $ ca_b `approxA` c_ab
  where
    Z:.id:.ih:.iw    = extent b
    Z:.kd:.kh:.kw = extent a

-- SoftMax
prop_SoftMaxSum1 (VecA a) = runIdentity $
  do probs <- forward1 a SoftMax
     sum <- sumAllP probs
     return $ sum `approx` 1

prop_SoftMaxMaxElemInvariant (VecA a) = runIdentity $
  do probs <- forward1 a SoftMax
     return $ maxIndex probs == maxIndex a

-- subtractOneAt
prop_subOneSum (VecA a) = runIdentity $
  do sa <- sumAllP a
     a' <- subtractOneAt 0 a
     sa' <- sumAllP a'
     return $ (sa - 1) `approx` sa'

-- backwards shape properties
prop_shapeInvariant (Layer3A l) (VolA x) = 2 `divs` w && 2 `divs` h  ==> runIdentity $
  do y <- forward3 x l
     (_, dx) <- backward3 l x y y 0 0
     return (extent x == extent dx)
  where
    _:.h:.w = extent x

return []
runtests = $quickCheckAll
