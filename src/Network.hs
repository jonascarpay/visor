{-# LANGUAGE TypeOperators #-}

module Network where

import Data.Array.Repa
import Layer

-- | A convolutional neural network for extracting features
--   from an image. May be unnecessary, only constrains
--   types on top layer and adds hyperparameters
data Network sIn =
  Network
    { -- | The regularization loss factor
      lambda :: Double,
      -- | The step size or learning rate
      delta :: Double,
      -- | Top layer of the network
      topLayer :: Layer (sIn:.Int:.Int:.Int) (sIn:.Int)
    }
