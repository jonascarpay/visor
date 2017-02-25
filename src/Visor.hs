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
{-# LANGUAGE AllowAmbiguousTypes #-}

module Visor where

import Types
import Vector
import Network
import Network.Label
import qualified Network.Runners as R
import Util
import Static
import qualified Static.Image as I
import Data.Proxy
import Data.Singletons.Prelude
import Data.Singletons.TypeLits
import Data.Array.Repa hiding (extract)

feedImage :: (WVector (Widgets a), Monad m) => Screenshot a -> Visor a -> m (Vec WLabel (Widgets a))
feedImage img (Visor visor) = do xs <- extract img
                                 forward visor xs

trainImage :: (WVector (Widgets a), Monad m)
           => Visor a
           -> Screenshot a
           -> Vec WLabel (Widgets a)
           -> m (Visor a, Loss)
trainImage (Visor v) shot ys =
  do xs <- extract shot
     (v', l) <- trainOnce v xs ys
     return (Visor v', l)


class WVector ws where
  extract   :: Monad m => Screenshot a -> m (Vec WInput ws)
  forward   :: Monad m => Vec WNetwork ws -> Vec WInput ws -> m (Vec WLabel ws)
  trainOnce :: Monad m
             => Vec WNetwork ws
             -> Vec WInput ws
             -> Vec WLabel ws
             -> m (Vec WNetwork ws, Loss)

instance WVector '[]
  where extract   _     = return $! Nil
        forward   _ _   = return $! Nil
        trainOnce _ _ _ = return $! (Nil, ((0,0),0))

instance (Widget a, WVector ts) => WVector (a ': ts) where

  extract shot =
    do crop <- extractWidget shot
       crops <- extract shot
       return$ crop :- crops

  forward (WNetwork n :- ns) (WInput x :- xs) =
    do y  <- R.forward n x
       ls <- forward ns xs
       let l = WLabel $ fromArray y
       return$ l:-ls

  trainOnce (WNetwork n :- ns) (WInput x :- xs) (WLabel l :- ls) =
    do let Params lparams = params :: Params a
       (n',  ((c,t),  l'))  <- R.trainOnce n lparams x (toArray l)
       (ns', ((cs, ts), ls')) <- trainOnce ns xs ls
       return$! (WNetwork n' :- ns', ((c+cs, t+ts), l' + ls'))

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
    delayedCrops = (\ !r -> (I.extract img r :: SArray D (ZZ ::. 3 ::. Height w ::. Width w))) <$> regions

    fn (Z :. n :. d :. y :. x) = let SArray crop = delayedCrops !! n in crop ! (Z :. d :. y :. x)

