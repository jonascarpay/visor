{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Visor
  ( forward
  ) where

import Types
import Static
import Static.Image

forward :: Monad m
        => Visor a
        -> SArray U (ZZ ::. 3 ::. ScreenWidth a ::. ScreenHeight a)
        -> m a
forward (Visor nvec) img = undefined

trainOnce :: Monad m
          => Visor a
          -> LabelVec (Widgets a)
          -> m Int
trainOnce = undefined

extractWidget :: forall a. Widget a => ScreenShot (Parent a) -> a -> SArray U (WInput a)
extractWidget = undefined

