{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

module Types where

import Data.Singletons.TypeLits
import Data.Singletons.Prelude

data Label (c :: Nat) = Label Int | NoParse

data Widget (sh :: [Nat]) where
  WNil  :: Widget '[]
  WCons :: Label c -> Widget cs -> Widget (c ': cs)

data WidgetBatch (n :: Nat) (sh :: [Nat]) where
  WBNil  :: WidgetBatch 0 sh
  WBCons :: Widget sh -> WidgetBatch n sh -> WidgetBatch (n :+ 1) sh

-- | If x1 and x2 could be the values for some x in two
--   subsequent screen polls, then x1 ->? x2
class Transitions a where
  (->?) :: a -> a -> Bool

class Transitions a => IsWidget a where
  toWidget   :: a -> Widget sh
  fromWidget :: Widget sh -> a

class Transitions a => IsWidgetBatch a where
  toWidgetBatch   :: a -> WidgetBatch n sh
  fromWidgetBatch :: WidgetBatch n sh -> a

wcat :: Widget as -> Widget bs -> Widget (as :++ bs)
wcat WNil           bs = bs
wcat (a `WCons` as) bs = a `WCons` wcat as bs
