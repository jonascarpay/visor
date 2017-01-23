{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

module Types where

import ConvNet
import Data.Singletons.TypeLits
import Data.Singletons.Prelude

data Label (c :: Nat) = Label Int | NoParse

data Widget (sh :: [Nat]) where
  WNil  :: Widget '[]
  WCons :: Label c -> Widget cs -> Widget (c ': cs)

data Widgets (n :: Nat) (sh :: [Nat]) where
  WBNil  :: Widgets 0 sh
  WBCons :: Widget sh -> (Double, Double) -> Widgets n sh -> Widgets (n :+ 1) sh

-- | If x1 and x2 could be the values for some x in two
--   subsequent screen polls, then x1 ->? x2.
--   This is used for discarding bad classifications in noisy streams.
class Transitions a where
  (->?) :: a -> a -> Bool

class Transitions a => WidgetData a where
  type Shape a :: [Nat]
  toWidget     :: a -> Widget (Shape a)
  fromWidget   :: Widget (Shape a) -> a

class Transitions a => WidgetBatch a where
  type Rows a :: Nat
  toWidgets   :: a -> Widgets (Rows a) sh
  fromWidgets :: Widgets (Rows a) sh -> a
  config      :: p a -> WidgetConfig a

data WidgetConfig a = WidgetConfig { defaultParams :: NetParams
                                   , resolution    :: Int
                                   , dimensions    :: (Double, Double)
                                   , netSpec       :: [LayerSpec]
                                   }

wcat :: Widget as -> Widget bs -> Widget (as :++ bs)
wcat WNil           bs = bs
wcat (a `WCons` as) bs = a `WCons` wcat as bs
