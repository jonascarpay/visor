module ConvNet where

import Control.Monad
import Volume
import Label

data ConvNet = ConvNet [Layer3] [Layer1]

-- | Defines the parameters for the construction of a convolutional layer
data LayerSpec
  = ConvS
      Int -- ^ Kernel size
      Int -- ^ Kernel count
  | ReLUS
  | PoolS

defaultSpec :: [LayerSpec]
defaultSpec = [ ConvS 64 32
              , ReLUS
              , ConvS 16 16
              , ReLUS
              , PoolS ]

initNet :: [LayerSpec] -- ^ Spec of the convolutional part of the network
        -> Int -- ^ Expected input width
        -> Int -- ^ Expected input height
        -> Int -- ^ Hidden layer size
        -> Int -- ^ RNG seed
        -> ConvNet
initNet = undefined

feed :: Monad m => ConvNet -> Volume -> m Label
feed (ConvNet l3s l1s) v = do vol <- foldConv v
                              vec <- flatten vol
                              y   <- foldFC vec
                              return $ maxIndex y
  where
    foldConv vol = foldM forward3 vol l3s
    foldFC   vec = foldM forward1 vec l1s

