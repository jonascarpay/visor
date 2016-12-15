{-# LANGUAGE TemplateHaskell #-}
module VolumeSpec where

import Test.QuickCheck
import Data.Array.Repa
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

prop_rotateInvolutionM (MatA a) = computeS (rotate (rotate a)) == a
_prop_rotateInvolutionV (VolA a) = computeS (rotate (rotate a)) == a
_prop_rotateInvolutionW (WgtA a) = computeS (rotate (rotate a)) == a

return []
runtests = $quickCheckAll
