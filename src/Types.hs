{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

module Types where

import Network.Label
import Data.Singletons.TypeLits
import Data.Singletons.Prelude.List

-- | If x1 and x2 could be values for some x in two
--   subsequent screen polls, then x1 ->? x2.
--   This is used for discarding bad classifications in noisy streams.
class Transitions a where
  (->?) :: a -> a -> Bool

-- | A GameState is a data type that fully describes a games' state.
class Transitions a => GameState a where
  widgets :: a -> WidgetVec (Widgets a)

  type Title  a       :: Symbol
  type ScreenWidth  a :: Nat -- ^ The width of a screen of this game in pixels.
                             --   This value and the height are mostly used to
                             --   scale the positions and dimensions of widgets
  type ScreenHeight a :: Nat
  type Widgets a      :: [*]

class Transitions a => Widget a where
  toLabel   :: a -> WLabel a
  fromLabel :: LabelParser a
  type Positions a :: [(Nat, Nat)]
  type DataShape a :: [Nat]
  type Width  a    :: Nat
  type Height a    :: Nat

type WLabel a = LabelComposite (Length (Positions a)) (DataShape a)

data WidgetVec (ws :: [*]) where
  WNil  :: WidgetVec '[]
  WCons :: Widget a
        => ! (WLabel a)
        -> ! (WidgetVec ws)
        -> WidgetVec (a ': ws)

-- | A data set defines a set of samples for some game
data Dataset a =
  Dataset
    { -- ^ Absolute paths to the images in the data set
      rootDir :: FilePath,
      parseFilename :: FilePath -> WidgetVec (Widgets a),
      -- ^ The rectangle to crop the images to. This should be the
      --   largest possible area that only captures the game screen.
      --   Nothing implies the entire image
      cropRect :: Maybe Rect,
      -- ^ Indicates the number of extra pixels we can crop off
      --   in all directions. This is used to apply a random
      --   translation to the image.
      wiggle :: Int,
      -- ^ Whether or not to apply random color distortion to the
      --   sample images
      distort :: Bool
    }

data Rect = Rect !Int !Int !Int !Int
