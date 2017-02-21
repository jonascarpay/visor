{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Visor
  ( forward
  , trainOnce
  ) where

import Types
import Vector
import Util
import Static
import Static.Image
import Data.Proxy
import Data.Singletons.Prelude
import Data.Singletons.TypeLits
import Data.Array.Repa hiding (extract)

forward :: Monad m
        => Visor a
        -> Screenshot a
        -> SArray U (ZZ ::. 3 ::. ScreenWidth a ::. ScreenHeight a)
        -> m a
forward (nvec) img = undefined

trainOnce :: Monad m
          => Visor a
          -> p a
          -> LabelVec a
          -> m Int
trainOnce = undefined

extractInputs :: forall a m. Monad m => Screenshot a -> m (InputVec a)
extractInputs img = undefined

extractWidget :: forall a m.
  ( ValidWidget a, Monad m
  ) => Proxy a -> Screenshot (Parent a) -> m (WInput a)

extractWidget Proxy (Screenshot img) = do sarr <- sComputeP$ sFromFunction fn
                                          return$ WInput sarr
  where
    ww  = fromInteger$ natVal (Proxy :: Proxy (Width  a))
    wh  = fromInteger$ natVal (Proxy :: Proxy (Height a))
    iw  = fromInteger$ natVal (Proxy :: Proxy (ScreenWidth  (Parent a)))
    ih  = fromInteger$ natVal (Proxy :: Proxy (ScreenHeight (Parent a)))
    wps = fromSing (sing :: Sing (Positions a))

    regions = (\ (x,y) -> Rect (fromInteger x/iw) (fromInteger y/ih) (ww/iw) (wh/ih)) <$> wps
    delayedCrops = (\ !r -> (extract img r :: SArray D (ZZ ::. 3 ::. Height a ::. Width a))) <$> regions

    fn (Z :. n :. d :. y :. x) = let SArray crop = delayedCrops !! n in crop ! (Z :. d :. y :. x)
