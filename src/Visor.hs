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
forward (Visor nvec) img = undefined

trainOnce :: Monad m
          => Visor a
          -> LabelVec (Widgets a)
          -> m Int
trainOnce = undefined

extractInputs :: forall a m. ToIVec (Widgets a) (Inputs (Widgets a)) a
              => Monad m => Screenshot a -> m (InputCrops a)
extractInputs img = toVec (Proxy :: Proxy (Widgets a)) img

extractWidget :: forall a m.
  ( ValidWidget a , Monad m
  ) => Proxy a -> Screenshot (Parent a) -> m (SArray U (WInput a))
extractWidget Proxy (Screenshot img) = sComputeP$ sFromFunction fn
  where
    ww  = fromInteger$ natVal (Proxy :: Proxy (Width  a))
    wh  = fromInteger$ natVal (Proxy :: Proxy (Height a))
    iw  = fromInteger$ natVal (Proxy :: Proxy (ScreenWidth  (Parent a)))
    ih  = fromInteger$ natVal (Proxy :: Proxy (ScreenHeight (Parent a)))
    wps = fromSing (sing :: Sing (Positions a))

    regions = (\ (x,y) -> Rect (fromInteger x/iw) (fromInteger y/ih) (ww/iw) (wh/ih)) <$> wps
    delayedCrops = (\ r -> (extract img r :: SArray D (ZZ ::. 3 ::. Height a ::. Width a))) <$> regions

    fn (Z :. n :. d :. y :. x) = let SArray crop = delayedCrops !! n in crop ! (Z :. d :. y :. x)

-- More elegant solution?
-- | Converts a list of widgets and a screenshot into an InputVector
class ToIVec (a :: [*]) (b :: [SMeasure]) c
  where toVec :: Monad m => p a -> Screenshot c -> m (ImageVec b)

instance ToIVec '[] '[] c
  where toVec _ _ = return INil

instance ( ToIVec ws is c
         , c ~ Parent w
         , WInput w ~ i
         , ValidWidget w
         ) => ToIVec (w ': ws) (i ': is) c
  where toVec _ img = do crop <- extractWidget (Proxy :: Proxy w) img
                         crops <- toVec (Proxy :: Proxy ws) img
                         return$! crop `ICons` crops
