{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ConvNet where

import Control.Monad
import qualified Data.Array.Repa as R
import Volume
import Label
import GHC.Generics (Generic)
import Data.Serialize

-- | A convolutional network for image processing. The [Layer3] part represents
--   the part of the network whose outputs are volumes, i.e. the convolutional
--   part of the network. The [Layer1] Part is the fully connected part of the
--   network. The reason there are multiple output layers is to leverage the
--   fact that often features look similar and can therefore reuse convolution
--   kernels.
data ConvNet = ConvNet [Layer3] [[Layer1]]
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
                             , label :: Label
                             } deriving (Eq, Show, Generic)
instance Serialize ConvSample

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
  deriving (Eq, Show)

initCNet :: [LayerSpec] -- ^ Spec of the convolutional part of the network
        -> Int -- ^ Input width
        -> Int -- ^ Input height
        -> [Int] -- ^ Output dimensionalities
        -> ConvNet
initCNet specs iw ih ds = ConvNet convs fcs
  where
    sq x = x^(2::Int)

    (k,convs) = unroll3 specs iw ih 3 9
    fcs = fmap (\(d,i) -> [randomFCLayer k d (99 + i), SoftMax]) (zip ds [1..])

    unroll3 :: [LayerSpec] -> Int -> Int -> Int -> Int -> (Int, [Layer3])
    unroll3 []             w h d _ = (d*w*h,[])
    unroll3 (ReLUS:ls)     w h d r = (ReLU :) <$> unroll3 ls w h d r
    unroll3 (PoolS:ls)     w h d r = (Pool :) <$> unroll3 ls (w `div` 2) (h `div` 2) d r
    unroll3 (ConvS s n:ls) w h d r =
      (randomConvLayer s d n w h r :) <$> unroll3 ls (w-s+1) (h-s+1) n (sq r)

feed :: Monad m => ConvNet -> Volume -> m [Label]
feed (ConvNet l3s l1ss) v = do vol <- foldConv v
                               vec <- flatten vol
                               ys  <- foldFC vec
                               return $ fmap maxIndex ys
  where
    foldConv vol = foldM forward3 vol l3s
    foldFC   vec = mapM (foldM forward1 vec) l1ss

train1 :: Monad m
       => [Layer1]
       -> Vector
       -> Label
       -> Double
       -> m (Vector, [Layer1], Double)
train1 [] x y _ = do dx <- subtractOneAt (fromLabel y) x
                     return (dx, [], dataLoss x y)
train1 (l:ls) x y α =
  do f <- forward1 x l
     (df, ls', loss) <- train1 ls f y α
     (l', dx) <- backward1 l x f df 0 α
     return (dx, l':ls', loss)

train3 :: Monad m
       => [Layer3]
       -> [[Layer1]]
       -> Volume
       -> Label
       -> Double
       -> m (Volume, [Layer3], [[Layer1]], [Double])
train3 [] l1ss x y α = do
  f <- flatten x
  rs <- forM l1ss (\l1s -> train1 l1s f y α)

  let (dfs, l1s', losses) = unzip3 rs
      df' = foldr1 (R.+^) (fmap R.delay dfs)

  df :: Vector <- R.computeP df'
  dx <- R.computeP $ R.reshape (R.extent x) df
  return (dx, [], l1s', losses)

train3 (l:ls) l1s x y α =
  do f <- forward3 x l
     (df, l3s', l1s', loss) <- train3 ls l1s f y α
     (l', dx) <- backward3 l x f df 0 α
     return (dx, l':l3s', l1s', loss)
