{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

module Types where

import Static

import Data.Vector.Unboxed
import Data.Array.Repa
import Data.Singletons.TypeLits
import Data.Singletons.Prelude.List

-- | If x1 and x2 could be values for some x in two
--   subsequent screen polls, then x1 ->? x2.
--   This is used for discarding bad classifications in noisy streams.
class Transitions a where
  (->?) :: a -> a -> Bool

class Transitions a => GameState a where
  fromFilename :: String -> a
  type Title  a :: Symbol
  type ScreenWidth  a :: Nat
  type ScreenHeight a :: Nat
  type WidgetConfig a :: Widgets

  type Positions a :: [(Nat, Nat)]
  type DataShape a :: [Nat]
  type Width  a    :: Nat
  type Height a    :: Nat

data Widgets where
  WNil :: Widgets
  WCons :: Nat
        -> Nat
        -> [Nat]
        -> Widgets


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
newtype LabelV       (t :: LabelType) = LabelV  {getLabelV  :: (Vector Double)}
newtype WidgetV      (t :: LabelType) = WidgetV {getWidgetV :: (Vector Double)}
newtype WidgetBatchA (t :: LabelType) = WidgetBatchA {getWidgetBatchA :: Array U DIM2 Double}
