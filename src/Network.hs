{-# LANGUAGE TypeOperators #-}

module Network where

import Layer
import Numeric.LinearAlgebra

-- | A convolutional neural network for extracting features
--   from an image. May be unnecessary, only constrains
--   types on top layer and adds hyperparameters
data Network =
  Network
    { -- | The regularization loss factor
      lambda :: Double,
      -- | The step size or learning rate
      delta :: Double,
      -- | Layers in the network
      layers :: [Layer]
    }

-- | Feed a network some matrix n * d where n is the number of
--   sample and d is the dimensionality of the network input.
--   The output has shape n * k, where each sample has a row of
--   class scores that sums to 1.
feed :: Matrix R -> Network -> Matrix R
feed x (Network _ _ ls) = foldl forward x ls
