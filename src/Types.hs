{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import Conduit

class Pretty a where
  pretty :: a -> String
-- | A GameState is a data type that fully describes a games' state.
class ( KnownSymbol (Title g)
      , KnownNat (ScreenWidth g)
      , KnownNat (ScreenHeight g)
      , Show g, Pretty g
      ) => GameState g where

  label   :: g -> LabelVec g
  delabel :: LabelVec g -> g
  rootDir :: Path g
  parse   :: Path g -> LabelVec g

  type Title  g       :: Symbol
  type ScreenWidth  g :: Nat -- ^ The width of a screen of this game in pixels.
                             --   This value and the height are mostly used to
                             --   scale the positions and dimensions of widgets
  type ScreenHeight g :: Nat
  type Widgets g      :: [*]

class ( KnownNat (Height w), KnownNat (Width w), KnownNat (Length (Positions w))
      , KnownNat (ScreenWidth (Parent w)), KnownNat (ScreenHeight (Parent w))
      , KnownNat (Sum (DataShape w)), KnownNat ((Sum (DataShape w)) :* (Length (Positions w)))
      , SingI (Positions w), Measure (InputShape w)
      , SingI (DataShape w)
      , KnownNat (SampleWidth w)
      , KnownNat (SampleHeight w)
      , KnownNat (Length (Positions w) :* SampleWidth w)
      , (NOutput (Network (InputShape w) (NetConfig w))) ~ (ZZ ::. Length (Positions w) ::. Sum (DataShape w))
      ) => Widget w where
  toLabel   :: w -> WLabel w
  fromLabel :: LabelParser w
  params    :: Params w

  -- Widget description
  type Positions w :: [(Nat, Nat)]
  type DataShape w :: [Nat]
  type Width     w :: Nat
  type Height    w :: Nat
  type Parent    w :: *

  -- Widget classifier configuration
  type SampleWidth  w :: Nat
  type SampleHeight w :: Nat
  type NetConfig    w :: [*]

newtype Params w = Params LearningParameters

type InputShape  w = ZZ ::. Length (Positions w) ::. 3 ::. SampleHeight w ::. SampleWidth w
type OutputShape w = LabelComposite (Length (Positions w)) (DataShape w)

type BatchInputShape  w n = ZZ ::. n :* Length (Positions w) ::. 3 ::. SampleHeight w ::. SampleWidth w
type BatchOutputShape w n = ZZ ::. n :* Length (Positions w) ::. Sum (DataShape w)

newtype WLabel w = WLabel   (OutputShape w)
deriving instance Serialize (OutputShape w) => Serialize (WLabel w)
deriving instance Creatable (OutputShape w) => Creatable (WLabel w)
deriving instance Show      (OutputShape w) => Show      (WLabel w)
deriving instance Eq        (OutputShape w) => Eq        (WLabel w)

newtype WNetwork w = WNetwork (Network (InputShape w) (NetConfig w))
deriving instance Serialize   (Network (InputShape w) (NetConfig w)) => Serialize (WNetwork w)
deriving instance Creatable   (Network (InputShape w) (NetConfig w)) => Creatable (WNetwork w)
deriving instance Show        (Network (InputShape w) (NetConfig w)) => Show      (WNetwork w)

newtype WInput w = WInput   (SArray U (InputShape w))
deriving instance Serialize (SArray U (InputShape w)) => Serialize (WInput w)
deriving instance Creatable (SArray U (InputShape w)) => Creatable (WInput w)
deriving instance Show      (SArray U (InputShape w)) => Show      (WInput w)

newtype WBatch n w = WBatch ( SArray U (BatchInputShape w n)
                            , SArray U (BatchOutputShape w n))

deriving instance ( Serialize (SArray U (BatchInputShape  w n))
                  , Serialize (SArray U (BatchOutputShape w n))
                  ) => Serialize (WBatch n w)

deriving instance ( Show (SArray U (BatchInputShape  w n))
                  , Show (SArray U (BatchOutputShape w n))
                  ) => Show (WBatch n w)

newtype LabelVec g = LabelVec (Vec WLabel (Widgets g))

deriving instance Show (Vec WLabel (Widgets g)) => Show (LabelVec g)
deriving instance Eq   (Vec WLabel (Widgets g)) => Eq   (LabelVec g)

type InputVec   g = Vec WInput   (Widgets g)
type NetworkVec g = Vec WNetwork (Widgets g)
type BatchVec n g = Vec (WBatch n) (Widgets g)
type BatchC n g = RTConduit (Screenshot g, LabelVec g) (Vec (WBatch n) (Widgets g))

newtype Visor game = Visor (NetworkVec game)
deriving instance Serialize (NetworkVec game) => Serialize (Visor game)
deriving instance Creatable (NetworkVec game) => Creatable (Visor game)

newtype Screenshot g = Screenshot BMP
newtype Path g = Path {unpath :: FilePath}
instance Show (Path g) where show (Path p) = p

type RTSource  a   = Source    (ResourceT IO) a
type RTConduit a b = Conduit a (ResourceT IO) b
type RTSink    a   = Sink    a (ResourceT IO) ()


