{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ConvNet where

import Volume
import Label
import Util
import Control.Monad
import GHC.Generics (Generic)
import qualified Data.Array.Repa as R
import Data.Serialize (Serialize)
import Control.Monad.Trans.State.Strict

-- | A convolutional network for image processing. The [Layer3] part represents
--   the part of the network whose outputs are volumes, i.e. the convolutional
--   part of the network. The [Layer1] Part is the fully connected part of the
--   network. The reason there are multiple output layers is to leverage the
--   fact that often features look similar and can therefore reuse convolution
--   kernels.
data ConvNet = ConvNet [Layer3] [Int]

  deriving Generic
instance Serialize ConvNet
-- TODO: A good future optimization is
-- `data Shape sh => ConvNet sh = ConvNet [Layer(3)] sh`
-- An FC layer can be modeled as a volumetric layer with output dimensions 1x1xn.
-- The shape gives the size of the output space, and a classification is some
-- point in that space. The size of the space affects what ranges of output
-- neurons is grouped into a single SoftMax classification. This is functionally
-- equivalent to the current implementation, but more extensible and likely
-- has better performance.

data ConvSample = ConvSample { sample :: Volume
                             , label  :: [Label]
                             } deriving (Eq, Show, Generic)
instance Serialize ConvSample

-- | When a widget occurs more than one in an image, extracting them
--   gives a sequence of samples rather than a single sample
type ConvSampleSequence = [ConvSample]
type VisorSample = [ConvSampleSequence]

instance Show ConvNet where
  show (ConvNet l3s l1s) = unlines $ ["ConvNet"] ++ l3str ++ [" -"] ++ l1str
    where
      indent str = "  " ++ str
      l3str = fmap (indent . show) l3s
      l1str = fmap (indent . show) l1s

-- | Defines the parameters for the construction of a convolutional layer
data LayerSpec
  = ConvS
      Int -- ^ Kernel size
      Int -- ^ Kernel count
  | ReLUS
  | PoolS
  | FCS Int
  deriving (Eq, Show)

-- TODO: enforce square inputs
initCNet :: [LayerSpec] -- ^ Spec of the convolutional part of the network
        -> Int -- ^ Input width
        -> Int -- ^ Input height
        -> [Int] -- ^ Output dimensionalities
        -> ConvNet
initCNet specs iw ih ds = ConvNet convs ds
  where
    sq x = x^(2::Int)

    convs = unroll3 specs iw ih 3 9

    unroll3 :: [LayerSpec] -> Int -> Int -> Int -> Int -> [Layer3]
    unroll3 []         w h d r = [randomConvLayer w d (sum ds) w h r]
    unroll3 (ReLUS:ls) w h d r = ReLU : unroll3 ls w h d r

    unroll3 (FCS n:ls) w h d r
      | w == h = unroll3 (ConvS w n:ls) w h d r
      | otherwise = error "Non-square input when constructing FC layer"

    unroll3 (PoolS:ls)     w h d r
      | 2 `divs` w && 2 `divs` h = Pool : unroll3 ls (w `div` 2) (h `div` 2) d r
      | otherwise = error "Non-even dimensions when constructing pooling layer"

    unroll3 (ConvS s n:ls) w h d r
      | w-s+1 > 0 && h-s+1 > 0 = randomConvLayer s d n w h r : unroll3 ls (w-s+1) (h-s+1) n (sq r)
      | otherwise = error "Convolution kernel is too large"

feed :: Monad m => ConvNet -> Volume -> m [Label]
feed (ConvNet l3s cs) v = do vol <- foldConv v
                             vec <- flatten vol
                             ys  <- softMax vec cs
                             return $ toLabel <$> getMaxima ys cs
  where
    foldConv vol = foldM forward3 vol l3s

feedThresholded :: Monad m => Double -> ConvNet -> Volume -> m [Label]
feedThresholded t (ConvNet l3s cs) v = do vol <- foldConv v
                                          vec <- flatten vol
                                          ys  <- softMax vec cs
                                          return $ getMaximaThresholded ys cs t
  where
    foldConv vol = foldM forward3 vol l3s

-- TODO: lenses
data TrainState = TrainState { network :: ConvNet
                             , learningRate :: Double
                             , regularizationLoss :: Double
                             , momentumFactor :: Double
                             , velocity :: [Layer3]
                             }

train :: Volume -> [Label] -> Trainer LossVector
train x y = do TrainState (ConvNet l3s cs) α λ γ vs <- get
               (_, deltas, lvec) <- getDeltas l3s x cs y
               (l3s', vs') <- applyDeltas deltas l3s vs α λ γ
               put $ TrainState (ConvNet l3s' cs) α λ γ vs'
               return lvec

applyDeltas :: Monad m
            => [Layer3] -- ^ Delta layers
            -> [Layer3] -- ^ Network layers
            -> [Layer3] -- ^ Velocity layers
            -> Double   -- ^ Learning rate
            -> Double   -- ^ Regularization loss
            -> Double   -- ^ Momentum factor
            -> m ([Layer3], [Layer3])
applyDeltas (dl:dls) (l:ls) (v:vs) α λ γ =
  do (ls', vs') <- applyDeltas dls ls vs α λ γ
     (l' , v')  <- applyDelta  dl  l  v  α λ γ
     return (l':ls', v':vs')

applyDeltas [] [] [] _ _ _ = return ([], [])
applyDeltas _  _  _  _ _ _ = error "Network size mismatch when updating layers"

type Trainer = State TrainState
type LossVector = [Double]

getDeltas :: Monad m
       => [Layer3] -- ^ Network layers
       -> Volume   -- ^ Input volume
       -> [Int]    -- ^ Softmax output cardinalities
       -> [Label]  -- ^ Correct labels
       -> m (Volume, [Layer3], [Double])

getDeltas [] x cs ys = do
  f <- flatten x
  p <- softMax f cs
  (df,losses) <- softMaxBackward p cs ys
  dx <- if any (>4) losses
           then R.computeP $ R.fromFunction (R.extent x) (const 0)
           else R.computeP $ R.reshape (R.extent x) df
  return (dx, [], losses)

getDeltas (l:ls) x cs ys =
  do f <- forward3 x l
     (df, dls, loss) <- getDeltas ls f cs ys
     (dl, dx) <- backward3 l x f df
     return (dx, dl:dls, loss)
