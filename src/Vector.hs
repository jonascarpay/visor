{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}

module Vector
  ( Vec (..)
  , vmap
  , NVec
  ) where

import Util
import Data.Functor.Identity
import Data.Serialize

data Vec :: (k -> *) -> [k] -> *
  where Nil  :: Vec fn '[]
        (:-) :: fn a
             -> ! (Vec fn as)
             -> Vec fn (a ': as)

infixr 7 :-

instance Creatable (Vec fn '[]) where
  seeded _ = Nil

instance Show (Vec a '[]) where
  show _ = "Nil"

instance (Show (f a), Show (Vec f ts)) => Show (Vec f (a ': ts)) where
  show (x :- xs) = show x ++ " :- " ++ show xs

instance (Creatable (fn a), Creatable (Vec fn as))
  => Creatable (Vec fn (a ': as)) where
  seeded s = seeded s :- seeded s

instance Serialize (Vec fn '[]) where
  put _ = return ()
  get   = return Nil

instance (Serialize (fn a), Serialize (Vec fn as))
  => Serialize (Vec fn (a ': as)) where

  put (x :- xs) =
    do put x
       put xs
  get =
    do x  <- get
       xs <- get
       return$! x :- xs

type NVec = Vec Identity

{-
  fmap' :: SMap a b as bs => (a -> b) -> NVec as -> NVec bs
  fmap' f = vmap (fmap f)
-}

class VMap a b as bs where
  vmap :: (fn1 a  ->     fn2 b)
    -> Vec fn1 as -> Vec fn2 bs

instance VMap a b '[] '[] where
  vmap :: (fn1 a -> fn2 b) -> Vec fn1 '[] -> Vec fn2 '[]
  vmap _ Nil = Nil

instance VMap a b as bs => VMap a b (a ': as) (b ': bs) where

  vmap :: (fn1 a -> fn2 b) -> Vec fn1 (a ': as) -> Vec fn2 (b ': bs)

  vmap f (x :- xs) = f x :- vmap f xs
