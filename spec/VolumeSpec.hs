{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module VolumeSpec where

import Prelude hiding (map)
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

prop_poolSum (VolA a) = once$
  h>2 && w>2 && h `mod` 2 == 0 && w `mod` 2 == 0
  ==> max (maxElem a) 0 == maxElem a'
  where
    maxElem = foldAllS max 0
    a' = runIdentity $ pool a
    Z:._:.h:.w = extent a

prop_poolBackpropShapeSumInvariant (VolA a) = runIdentity $
  do p :: Volume <- pool a
     dp :: Volume <- computeP $ map (1/) p
     da <- poolBackprop a p dp
     sdp <- sumAllP dp
     sda <- sumAllP da
     return $ sda == sdp


prop_zeroPadAssoc (MatA a) (Positive (Small n1)) (Positive (Small n2)) = once$
    (computeS . zeropad n1 $ (computeS . zeropad n2 $ a :: Matrix) :: Matrix)
     == (computeS . zeropad (n1+n2)) a

prop_zeroPadSumInvariant (MatA a) = sumAllS (computeS (zeropad 1 a) :: Matrix) == sumAllS a

prop_vecMult (VecA a) (VecA b) = abs (sumAllS a * sumAllS b - sumAllS (a `vvmult` b)) < 1e-3

prop_corrExtent (WgtA a) (VolA b) = once$
    id == kd && ih >= kh && iw >= kw
    ==> shOut == (Z:.kn:.ih-kh+1:.iw-kw+1)
  where
    shOut = extent out
    Z:.id:.ih:.iw     = extent b
    Z:.kn:.kd:.kh:.kw = extent a
    out = runIdentity $ a `corr` b

return []
runtests = $quickCheckAll
