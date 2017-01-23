{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}

module Conversions where

import Types
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import Control.Monad.ST

-- | Conversions should be homomorphisms (I think)
class Convertible a b where
  convert :: a -> b

instance KnownNat n => Convertible (Label n) (LabelV 'OneHot) where
  convert label = runST $ do let len = 1 + (fromIntegral $ natVal label :: Int)
                                 i = case label of
                                       NoParse -> 0
                                       Label x -> x + 1
                             vec <- UM.new len
                             UM.write vec i 1
                             vec' <- U.unsafeFreeze vec
                             return $! LabelV vec'
