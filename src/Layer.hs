module Layer where

import Numeric.LinearAlgebra
import Util

-- | A layer consists of a number of weights, a bias vector,
--   and some activation function.
data Layer = L { -- | Weight matrix
                 weights :: Matrix R,
                 -- | Bias vector
                 bias :: Vector R,
                 -- | The activation function introduces some nonlinearity
                 --   in the network. Empirical evidence suggests that
                 --   ReLU activation functions are most effective for
                 --   hidden layers, while the final classification layer
                 --   should use a softMax activation function because of
                 --   its convenient mathematical properties.
                 activationFunction :: ActivationFunction
               } deriving Show

-- | Apply a matrix input to a layer. The output consists of the weight
--   application followed by the activation function for this layer.
forward :: Matrix R -> Layer -> Matrix R
forward x (L w b af) = activate af $ x <> w + asRow b

backward :: Layer -- ^ Layer to backpropagate through
         -> Matrix R -- ^ Input for that layer during forward pass
         -> Matrix R -- ^ Output of that layer during forward pass
         -> Matrix R -- ^ Error gradient on the output of this
                     --   layer (or equivalently the input of the
                     --   next layer)
         -> Double -- ^ Delta/Step size/Learning rate
         -> Double -- ^ Lambda/Regularization loss
         -> (Layer, Matrix R) -- ^ Updated layer with new weights,
                              --   and error gradient on the input
                              --   of this layer/output of the
                              --   previous layer

backward (L w b af) x p dp d r = (l', dx)
  where
    df, dw, dw', dx :: Matrix R
    db, db' :: Vector R
    -- Error gradient on the output of the weights before
    -- the activation function
    df = diff af p dp

    -- Error gradient for the weights themselves
    dw = tr x <> df + scale r w
    dw' = d `scale` dw

    -- Error gradient for the bias vector
    db = colSums df
    db' = d `scale` db

    -- Error gradient on the layer input matrix
    dx = df <> tr w

    -- Updated layer. The error gradient on the weights
    -- is subtracted from the weights in order to adjust
    -- the weights in the direction of decreasing error
    l' = L (w - dw') (b - db') af



-- | Possible activation functions for fully connected layers.
data ActivationFunction
  = SoftMax -- ^ Used to turn a rows into class scores. Class
            --   scores sum to one which gives them the
            --   useful interpretation of being probabilities.
            --   The softMax activation function is popular
            --   because of its convenient mathematical
            --   properties, such as a very easily calculated
            --   derivative.
  | ReLU -- ^ Simply turns all negative inputs into 0, while
         --   leaving positive inputs untouched. Popular
         --   because it is easily calculated, easy to
         --   derive a gradient for, and has been proven
         --   empirically to provide good results.
  deriving Show

-- | Gives an actual function on a matrix given an activation
--   function term
activate :: ActivationFunction
         -> Matrix R -- ^ Intermediate layer output after
                     --   applying the weights
         -> Matrix R -- ^ Final layer output
activate ReLU    m = m * step m
activate SoftMax m = normalizeRows . cmap exp $ m

-- | Derivative of an activation function
diff :: ActivationFunction -- ^ Activation function in question
     -> Matrix R -- ^ Output of the activation function. For most
                 --   commonly used activation functions the output
                 --   occurs in the derivative, so supplying this
                 --   directly makes computation more efficient
                 --   then it would be if we recalculate it.
     -> Matrix R -- ^ Error gradient on the output
     -> Matrix R -- ^ Error gradient on the output of the weights
diff SoftMax p dp = (p - dp) / fromIntegral (rows p)
diff ReLU    p dp = dp * step p

regularizationLoss :: Double -- ^ Lamda/Regularization loss factor
                   -> Layer -- ^ Layer to calculate reg. loss for
                   -> Double -- ^ Regularization loss
regularizationLoss r (L w _ _) = 0.5 * r * sumElements (w*w)

dataLoss :: Matrix R -- ^ Network output
         -> Matrix R -- ^ Desired network output
         -> Double -- ^ Data loss
dataLoss p y = avgRowSum . mask . negLog $ p
  where
    -- Sets all non-correct class scores to 0. Assumes y is
    -- 0 of incorrect classses and 1 for correct classes
    mask = (*y)
    negLog = cmap (negate . log)
