module Network where

-- | A convolutional neural network for extracting features
-- from an image
data Network shIn shOut =
  Network
    { -- | The regularization loss factor
      lambda :: Double,
      -- | The step size or learning rate
      delta :: Double,
      -- | Convolutional part of the network
      convLayers :: [Layer3],
      -- | Neural part of the network
      neurLayers :: [Layer2]
    }

data Layer3
data Layer2

