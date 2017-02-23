{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}

module Types where

import Util
import Network
import Vector
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
class ( Transitions a
      , KnownSymbol (Title a)
      , KnownNat (ScreenWidth a)
      , KnownNat (ScreenHeight a)
      ) => GameState a where

  labels  :: a -> LabelVec a
  dataset :: Dataset a

  type Title  a       :: Symbol
  type ScreenWidth  a :: Nat -- ^ The width of a screen of this game in pixels.
                             --   This value and the height are mostly used to
                             --   scale the positions and dimensions of widgets
  type ScreenHeight a :: Nat
  type Widgets a      :: [*]

class ( KnownNat (Height a), KnownNat (Width a), KnownNat (Length (Positions a))
      , KnownNat (ScreenWidth (Parent a)), KnownNat (ScreenHeight (Parent a))
      , KnownNat (Sum (DataShape a)), KnownNat ((Sum (DataShape a)) :* (Length (Positions a)))
      , SingI (Positions a), Measure (InputShape a), Transitions a
      , SingI (DataShape a)
      , NOutput (Network (InputShape a) (NetConfig a)) ~ (ZZ ::. Length (Positions a) ::. Sum (DataShape a))
      ) => Widget a where
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

-- | A `WLabel a` contains a label for widget `a`

newtype WLabel   a = WLabel (LabelComposite (Length (Positions a)) (DataShape a))
deriving instance Serialize (LabelComposite (Length (Positions a)) (DataShape a)) => Serialize (WLabel a)
deriving instance Creatable (LabelComposite (Length (Positions a)) (DataShape a)) => Creatable (WLabel a)
deriving instance Show      (LabelComposite (Length (Positions a)) (DataShape a)) => Show      (WLabel a)

newtype WNetwork a = WNetwork (Network (InputShape a) (NetConfig a))
deriving instance Serialize (Network (InputShape a) (NetConfig a)) => Serialize (WNetwork a)
deriving instance Creatable (Network (InputShape a) (NetConfig a)) => Creatable (WNetwork a)
deriving instance Show      (Network (InputShape a) (NetConfig a)) => Show      (WNetwork a)

newtype WInput a = WInput (SArray U (InputShape a))
deriving instance Serialize (SArray U (InputShape a)) => Serialize (WInput a)
deriving instance Creatable (SArray U (InputShape a)) => Creatable (WInput a)
deriving instance Show      (SArray U (InputShape a)) => Show      (WInput a)

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
