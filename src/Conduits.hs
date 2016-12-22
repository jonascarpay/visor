{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}

module Conduits ( module Conduit
                , module Conduits
                ) where

import Conduit
import qualified Data.Conduit.Combinators as CC
import Control.Monad

-- TODO: Write Conduit typeclass for source/sink

foldl1' :: (Monad m) => (b -> b -> b) -> ConduitM b o m b
foldl1' f = do Just x <- CC.foldl1 f
               return x

loopC :: Monad m => m a -> m b
loopC c = c >> loopC c

replicateCC :: Monad m => Int -> ConduitM o o m ()
replicateCC n = awaitForever $ \x -> replicateM n (yield x)
