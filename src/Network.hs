{-# LANGUAGE TypeOperators #-}

module Network where

import Layer
import Util
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
feed :: Network -> Matrix R -> Matrix R
feed (Network _ _ ls) x = foldl forward x ls

-- | Train a network with one forward/backward pass cycle.
train :: Network -- ^ Network to train
      -> Matrix R -- ^ Input to the network
      -> Matrix R -- ^ Desired output given this input
      -> Network -- ^ Network with updated weights
train (Network r d ls) x y = let (_, ls') = go ls x y
                              in Network r d ls'
  where
    go :: [Layer] -> Matrix R -> Matrix R -> (Matrix R, [Layer])
    go []     x y = (y, [])
    go (l:ls) x y = let p = forward x l
                        (dp, ls') = go ls p y
                        (l', dx) = backward l x p dp d r
                     in (dx, l':ls')

loss :: Network
     -> Matrix R
     -> Matrix R
     -> Double
loss n@(Network r _ ls) x y = dLoss + rLoss
  where dLoss = dataLoss (feed n x) y
        rLoss = sum . map (regularizationLoss r) $ ls

-- | Calculates the accuracy of the network as a probability.
--   A class is considered the guess for some sample if the
--   it has a probability of at least 0.5.
accuracy :: Network -- ^ Network to test
         -> Matrix R -- ^ Input data
         -> Matrix R -- ^ Desired output
         -> Double -- ^ Probability of a correct classification
accuracy net x y = avgRowSum $ p' * y
  where
    p = feed net x
    p' = step $ p - 0.5

-- | Initialize a new network with given parameters. The weights
--   are normally distributed with mu = 0 and sigma = 0.01.
--   Hidden layers are given a ReLU activation function, while
--   the output layer has a SoftMax activation function.
initNet :: Int -- ^ Input dimensionality, or, equivalently,
               --   number of features
        -> Int -- ^ Output dimensionality, or number of classes
        -> [Int] -- ^ List of neurons in hidden layers. Giving
                 --   an empty list means the network is a simple
                 --   linear classifier.

        -> Double -- Lambda, or regularization loss
        -> Double -- Delta, or learning rate/step size
        -> IO Network
initNet d k h lambda delta = Network lambda delta <$> sequence (go d h k)
  where
    go :: Int -> [Int] -> Int -> [IO Layer]
    go d []     k = [lInit d k SoftMax]
    go d (h:hs) k =  lInit d h ReLU : go h hs k

    wInit :: Int -> Int -> IO (Matrix Double)
    wInit d k = (*0.01) <$> randn d k

    lInit :: Int -> Int -> ActivationFunction -> IO Layer
    lInit d k af = do w <- wInit d k
                      return $ L w (konst 0 k) af

