{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}

module Types where

import Network
import Static
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
  labels :: a -> LabelVec (Widgets a)

  type Title  a       :: Symbol
  type ScreenWidth  a :: Nat -- ^ The width of a screen of this game in pixels.
                             --   This value and the height are mostly used to
                             --   scale the positions and dimensions of widgets
  type ScreenHeight a :: Nat
  type Widgets a      :: [*]

class Transitions a => Widget a where
  toLabel   :: a -> WidgetLabel a
  fromLabel :: LabelParser a

  -- Widget description
  type Positions a :: [(Nat, Nat)]
  type DataShape a :: [Nat]
  type Width     a :: Nat
  type Height    a :: Nat

  -- Widget classifier configuration
  type SampleWidth  a :: Nat
  type SampleHeight a :: Nat
  type NetConfig    a :: [*]

type WidgetLabel a   = LabelComposite (Length (Positions a)) (DataShape a)
type WNetwork a = Network (ZZ ::. Length (Positions a) ::. 3 ::. SampleWidth a ::. SampleHeight a)
                          (NetConfig a)

type Networks (gameState :: *) = Networks' (Widgets gameState)
type family Networks' a :: [*] where
  Networks' '[]       = '[]
  Networks' (w ': ws) = WNetwork w ': Networks' ws

data LabelVec (ws :: [*]) where
  WNil  :: LabelVec '[]
  WCons :: Widget a
        => ! (WidgetLabel a)
        -> ! (LabelVec ws)
        -> LabelVec (a ': ws)

data NetworkVec (ws :: [*]) where
  VNil  :: NetworkVec '[]
  VCons :: ! (Network i ls)
        -> ! (NetworkVec ns)
        -> NetworkVec (Network i ls ': ns)

newtype Visor a = Visor (NetworkVec (Networks a))

-- | A data set defines a set of samples for some game
data Dataset a =
  Dataset
    { -- ^ Absolute paths to the images in the data set
      rootDir :: FilePath,
      parseFilename :: FilePath -> LabelVec (Widgets a),
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
