{-# LANGUAGE TypeOperators #-}

module Network where

import Layer

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
