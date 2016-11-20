{-# LANGUAGE GADTs #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE TypeOperators #-}

module Layer where

import Data.Array.Repa

type Data r s = Array r s Double
type Vector r = Array r DIM1 Double
type Matrix r = Array r DIM2 Double
type Volume r = Array r DIM3 Double

data Layer sIn sOut where
  FC :: { weights :: Matrix U,
          bias    :: Vector U,
          next    :: Layer (s:.Int) sOut
        } -> Layer (s:.Int) sOut

  SoftMax :: Layer (s:.Int) (s:.Int)
  Flatten :: Layer (s:.Int) s

forward :: Data U sIn -> Layer sIn sOut -> Data D sOut
forward = undefined
