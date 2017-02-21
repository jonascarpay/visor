{-# LANGUAGE KindSignatures #-}
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

module Types where

import Network
import Vector
import Util
import Static
import Static.Image
import Network.Label
import Data.Singletons.Prelude
import Data.Singletons.TypeLits
import Data.Singletons.Prelude.List
import Data.Serialize

-- | If x1 and x2 could be values for some x in two
--   subsequent screen polls, then x1 ->? x2.
--   This is used for discarding bad classifications in noisy streams.
class Transitions a where
  (->?) :: a -> a -> Bool

-- | A GameState is a data type that fully describes a games' state.
class Transitions a => GameState a where
  labels :: a -> LabelVec a

  type Title  a       :: Symbol
  type ScreenWidth  a :: Nat -- ^ The width of a screen of this game in pixels.
                             --   This value and the height are mostly used to
                             --   scale the positions and dimensions of widgets
  type ScreenHeight a :: Nat
  type Widgets a      :: [*]

class Transitions a => Widget a where
  toLabel   :: a -> WLabel a
  fromLabel :: LabelParser a

  -- Widget description
  type Positions a :: [(Nat, Nat)]
  type DataShape a :: [Nat]
  type Width     a :: Nat
  type Height    a :: Nat
  type Parent    a :: *

  -- Widget classifier configuration
  type SampleWidth  a :: Nat
  type SampleHeight a :: Nat
  type NetConfig    a :: [*]

type InputShape a = ZZ ::. Length (Positions a) ::. 3 ::. SampleWidth a ::. SampleHeight a

type ValidWidget a =
  ( Widget a, KnownNat (Height a), KnownNat (Width a)
  , KnownNat (ScreenWidth (Parent a)), KnownNat (ScreenHeight (Parent a))
  , SingI (Positions a), Measure (InputShape a))

-- | A `WLabel a` contains a label for widget `a`
newtype WLabel   a = WLabel (LabelComposite (Length (Positions a)) (DataShape a))
newtype WInput   a = WInput (SArray U (InputShape a))
newtype WNetwork a = WNetwork (Network (InputShape a) (NetConfig a))

type LabelVec   a = Vec WLabel   (Widgets a)
type InputVec   a = Vec WInput   (Widgets a)
type NetworkVec a = Vec WNetwork (Widgets a)

type Visor game   = NetworkVec game

-- | A data set defines a set of samples for some game
data Dataset a =
  Dataset
    { rootDir :: FilePath -- ^ Absolute paths to the images in the data set
    , parseFilename :: FilePath -> LabelVec a
    }

newtype Screenshot a = Screenshot BMP
