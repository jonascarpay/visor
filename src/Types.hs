{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

module Types where

import Data.Singletons.TypeLits
import Data.Singletons.Prelude

data Label (c :: Nat) = Label Int | NoParse

data WidgetData (sh :: [Nat]) where
  WNil  :: WidgetData '[]
  WCons :: Label c -> WidgetData cs -> WidgetData (c ': cs)

data WidgetBatch (n :: Nat) (sh :: [Nat]) where
  WBNil  :: WidgetBatch 0 sh
  WBCons :: WidgetData sh -> WidgetBatch n sh -> WidgetBatch (n :+ 1) sh

-- | If x1 and x2 could be the values for some x in two
--   subsequent screen polls, then x1 ->? x2
class Transitions a where
  (->?) :: a -> a -> Bool

class Transitions a => Widget a where
  toWidget   :: a -> WidgetData sh
  fromWidget :: WidgetData sh -> a

class Transitions a => IsWidgetBatch a where
  toWidgetBatch   :: a -> WidgetBatch n sh
  fromWidgetBatch :: WidgetBatch n sh -> a

wcat :: WidgetData as -> WidgetData bs -> WidgetData (as :++ bs)
wcat WNil           bs = bs
wcat (a `WCons` as) bs = a `WCons` wcat as bs
