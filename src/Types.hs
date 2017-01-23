{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

module Types
  ( module Types
  , module Data.Singletons.TypeLits
  ) where

import Data.Vector.Unboxed
import Data.Singletons.TypeLits
import Data.Singletons.Prelude

-- | A label for some image classification. We make
--   explicit that classification may fail, by
--   returning NoParse. This is useful for indicating
--   that a feature (does|did) not occur on screen.
data Label (c :: Nat) = Label Int | NoParse
  deriving (Eq, Show)

data Widget (sh :: [Nat]) where
  WNil  :: Widget '[]
  WCons :: Label c -> Widget cs -> Widget (c ': cs)

data Widgets (n :: Nat) (sh :: [Nat]) where
  WBNil  :: Widgets 0 sh
  WBCons :: Widget sh -> Widgets n sh -> Widgets (n :+ 1) sh

class GameState a where

-- | If x1 and x2 could be the values for some x in two
--   subsequent screen polls, then x1 ->? x2.
--   This is used for discarding bad classifications in noisy streams.
class Transitions a where
  (->?) :: a -> a -> Bool

class Transitions a => WidgetData a where
  type Size a :: [Nat]
  toWidget     :: a -> Widget (Size a)
  fromWidget   :: Widget (Size a) -> a

class Transitions a => WidgetBatch a where
  type Rows a :: Nat
  toWidgets   :: a -> Widgets (Rows a) sh
  fromWidgets :: Widgets (Rows a) sh -> a
  config      :: p a -> WidgetConfig a

data WidgetConfig a where
  WidgetConfig :: WidgetBatch a
               => { defaultParams :: NetParams
                  , resolution    :: Int
                  , dimensions    :: (Double, Double)
                  , netSpec       :: [LayerSpec]
                  , positions     :: PVec (Rows a)
                  } -> WidgetConfig a

data PVec (n :: Nat) where
  PNil  :: PVec 0
  PCons :: (Double, Double) -> PVec n -> PVec (n :+ 1)

-- | A data set defines a set of samples for some game
data Dataset a =
  Dataset
    { -- ^ Absolute paths to the images in the data set
      rootDir :: FilePath,
      parseFilename :: FilePath -> a,
      -- ^ The rectangle to crop the images to. This should be the
      --   largest possible area that only captures the game screen.
      --   Nothing implies the entire image
      cropRect :: Maybe (Rect Int),
      -- ^ Indicates the number of extra pixels we can crop off
      --  in all directions. This is used to apply a random
      --  translation to the image.
      wiggle :: Int,
      -- ^ Whether or not to apply random color distortion to the
      --   sample images
      distort :: Bool
    }

data Rect a = Rect { rx :: a
                   , ry :: a
                   , rw :: a
                   , rh :: a }

data NetParams = NetParams { learningRate :: Double
                           , regularizationLoss :: Double
                           , momentumFactor :: Double
                           }

-- | Defines the parameters for the construction of a convolutional layer
data LayerSpec
  = ConvS
      Int -- ^ Kernel size
      Int -- ^ Kernel count
  | ReLUS
  | PoolS
  | FCS Int
  deriving (Eq, Show)

data LabelType = OneHot | Probabilities
newtype LabelV (t :: LabelType) = LabelV (Vector Double)
