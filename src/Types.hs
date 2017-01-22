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

wcat :: Widget as -> Widget bs -> Widget (as :++ bs)
wcat WNil           bs = bs
wcat (a `WCons` as) bs = a `WCons` wcat as bs
