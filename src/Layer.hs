{-# LANGUAGE GADTs #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Layer where

import Data.Array.Repa

type Data r s = Shape s => Array r s Double
type Vector r = Data r DIM1
type Matrix r = Data r DIM2
type Volume r = Data r DIM3

data Layer sIn sOut where
  FC :: { weights :: Matrix U,
          bias    :: Vector U,
          next    :: Layer (s:.Int) sOut
        } -> Layer (s:.Int) sOut

  SoftMax :: Layer (s:.Int) (s:.Int)
  Flatten :: Layer (q:.Int:.Int) sOut -> Layer (q:.Int) sOut

forward :: ( Shape s
           )=>
  Data U (s:.Int:.Int) -> Layer (s:.Int:.Int) sOut -> Data D sOut
forward x (Flatten l') = let (sh:.n:.m) = extent x in undefined
