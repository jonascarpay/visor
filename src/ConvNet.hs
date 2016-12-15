module ConvNet where

import Control.Monad
import Volume
import Label

data ConvNet = ConvNet [Layer3] [Layer1]

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

defaultSpec :: [LayerSpec]
defaultSpec = [ ConvS 64 32
              , ReLUS
              , ConvS 16 16
              , ReLUS
              , PoolS ]

initCNet :: [LayerSpec] -- ^ Spec of the convolutional part of the network
        -> Int -- ^ Input width
        -> Int -- ^ Input height
        -> Int -- ^ Output dimensionality
        -> ConvNet
initCNet specs iw ih d = ConvNet convs fcs
  where
    sq x = x^(2::Int)

    (k,convs) = unroll3 specs iw ih 3 9
    fcs = [randomFCLayer k d 99, SoftMax]

    unroll3 :: [LayerSpec] -> Int -> Int -> Int -> Int -> (Int, [Layer3])
    unroll3 []             w h _ _ = (w*h,[])
    unroll3 (ReLUS:ls)     w h d r = (ReLU :) <$> unroll3 ls w h d r
    unroll3 (PoolS:ls)     w h d r = (Pool :) <$> unroll3 ls (w `div` 2) (h `div` 2) d r
    unroll3 (ConvS s n:ls) w h d r =
      (randomConvLayer s s d n (w-s+1) (h-s+1) r :) <$> unroll3 ls (w-s+1) (h-s+1) n (sq r)

feed :: Monad m => ConvNet -> Volume -> m Label
feed (ConvNet l3s l1s) v = do vol <- foldConv v
                              vec <- flatten vol
                              y   <- foldFC vec
                              return $ maxIndex y
  where
    foldConv vol = foldM forward3 vol l3s
    foldFC   vec = foldM forward1 vec l1s

