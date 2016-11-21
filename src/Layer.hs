module Layer where

import Numeric.LinearAlgebra

-- | Possible activation functions for fully connected layers.
data ActivationFunction
  = SoftMax
  | ReLU
  deriving Show

activate :: ActivationFunction -> Matrix R -> Matrix R
activate = undefined

-- | A layer consists of a number of weights, a bias vector,
--   and some activation function.
data Layer = L { -- | Weight matrix
                 weights :: Matrix R,
                 -- | The bias vector is represented by a 1*k matrix
                 --   instead of a vector type
                 bias :: Matrix R,
                 -- | The activation function introduces some nonlinearity
                 --   in the network. Empirical evidence suggests that
                 --   ReLU activation functions are most effective for
                 --   hidden layers, while the final classification layer
                 --   uses a softMax activation function because of its
                 --   convenient mathematical properties.
                 activationFunction :: ActivationFunction
               } deriving Show
