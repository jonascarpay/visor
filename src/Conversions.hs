{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}

-- TODO: Specialization/inlining hints
module Conversions where

import Types
import Data.Singletons.Prelude.List
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

-- TODO: This could be faster if the intermediate vectors
--       constructed from labels were skipped. I am leaving
--       the slow implementation for now, as I think this
--       is only called when generating batches, which
--       does not need to be fast
instance Convertible (Widget sh) (WidgetV 'OneHot) where
  convert w = WidgetV . U.concat . fmap getLabelV $ toList w
    where
      toList :: Widget sh' -> [LabelV 'OneHot]
      toList WNil = []
      toList (WCons l ws) = convert l : toList ws
