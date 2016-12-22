{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}

module Conduits ( module Conduit
                , module Conduits
                ) where

import Conduit
import qualified Data.Conduit.Combinators as CC
import Control.Monad

loopC :: Monad m => m a -> m b
loopC c = c >> loopC c
