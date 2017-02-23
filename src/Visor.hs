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

class WVector ws where
  extract'  :: Monad m => Screenshot a -> m (Vec WInput ws)
  forward'  :: Monad m => Vec WNetwork ws -> Vec WInput ws -> m (Vec WLabel ws)
  trainOnce' :: Monad m
             => LearningParameters
             -> Vec WNetwork ws
             -> Vec WInput ws
             -> Vec WLabel ws
             -> m (Vec WNetwork ws, Loss)

instance WVector '[]
  where extract'   _       = return $! Nil
        forward'   _ _     = return $! Nil
        trainOnce' _ _ _ _ = return $! (Nil, (0,0))

instance (Widget a, WVector ts) => WVector (a ': ts) where

  extract' shot =
    do crop <- extractWidget shot
       crops <- extract' shot
       return$ crop :- crops

  forward' (WNetwork n :- ns) (WInput x :- xs) =
    do y  <- R.forward n x
       ls <- forward' ns xs
       let l = WLabel $ fromArray y
       return$ l:-ls

  trainOnce' params (WNetwork n :- ns) (WInput x :- xs) (WLabel l :- ls) =
    do (n',  (p',  l'))  <- R.trainOnce n params x (toArray l)
       (ns', (ps', ls')) <- trainOnce' params ns xs ls
       return$! (WNetwork n' :- ns', (undefined p' ps', l' + ls'))

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

