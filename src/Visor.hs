{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE PolyKinds #-}
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

extractWidget :: forall w s m.
  ( Widget w, Monad m
  ) => Screenshot s -> m (WInput w)

extractWidget (Screenshot img) = WInput <$> sComputeP (sFromFunction fn)
  where
    ww  = fromInteger$ natVal (Proxy :: Proxy (Width  w))
    wh  = fromInteger$ natVal (Proxy :: Proxy (Height w))
    iw  = fromInteger$ natVal (Proxy :: Proxy (ScreenWidth  (Parent w)))
    ih  = fromInteger$ natVal (Proxy :: Proxy (ScreenHeight (Parent w)))
    wps = fromSing (sing :: Sing (Positions w))

    regions = (\ (x,y) -> Rect (fromInteger x/iw) (fromInteger y/ih) (ww/iw) (wh/ih)) <$> wps
    delayedCrops = (\ !r -> (extract img r :: SArray D (ZZ ::. 3 ::. Height w ::. Width w))) <$> regions

    fn (Z :. n :. d :. y :. x) = let SArray crop = delayedCrops !! n in crop ! (Z :. d :. y :. x)

class Extract ts where
  extract' :: Monad m => Screenshot a -> m (Vec WInput ts)

instance Extract '[]
  where extract' _ = return Nil

instance (Widget a, Extract ts) => Extract (a ': ts)
  where extract' shot = do crop <- extractWidget shot
                           crops <- extract' shot
                           return$ crop :- crops
