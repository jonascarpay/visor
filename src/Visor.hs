module Visor where

import ConvNet
import Control.Monad

newtype Visor = Visor [ConvNet]

-- | Specialized training function for monadic folding. The List of doubles
--   accumulates losses.
train' :: Monad m => Double -> (ConvNet, [[Double]]) -> ConvSample -> m (ConvNet, [[Double]])
train' α (ConvNet l3s l1ss, losses) (ConvSample i o) =
  do (_, l3s', l1ss', loss) <- train3 l3s l1ss i o α
     return (ConvNet l3s' l1ss', loss:losses)

trainVisor :: Monad m => Visor -> [[ConvSample]] -> m (Visor, [[[Double]]])
trainVisor (Visor nets) css = do zs <- z
                                 let (nets, losses) = unzip zs
                                 return (Visor nets, losses)
  where
    f :: Monad m => ConvNet -> [ConvSample] -> m (ConvNet, [[Double]])
    f net cs = foldM (train' 1e-3) (net, []) cs
    z = zipWithM f nets css