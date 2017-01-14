{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Game where

import Util
import Label
import ConvNet

data GameConfig = GameConfig { defaultParams :: NetParams
                             , widgets   :: [Widget]
                             }

class GameState a where
  fromFilename  :: String -> a
  fromLabel     :: [Label] -> a
  toLabel       :: a -> [Label]
  validate      :: a -> a -> Bool
  config        :: p a -> GameConfig


-- | A widget represents a (possibly repeating) rectangular area of
--   the screen containing information to be extracted.
data Widget =
  Widget
    { -- ^ The resolution at which this is fed to the network. Higher means
      --   slower convergence, easier overfitting, and worse performance.
      --   Usually, we want the lowest value at which features are still clearly
      --   distinguishable. Because the convolutional network only accepts square
      --   inputs, the resolution is the same for the x and y axes.
      resolution :: Int,
      -- ^ The x, y positions at which this widget occurs on the screen. Values
      --   should be relative between 0 and 1. The coordinate corresponds to
      --   the location of the top left corner.
      position :: [(Double, Double)],
      -- ^ The dimensions of the widget, again, both between 0 and 1.
      dimensions :: (Double, Double),
      -- ^ The length of this list equals the amount of features to extract from
      --   a widget, and the values indicate how many possible values that feature
      --   takes. A digit, for example, would have a cardinality of 10. Note that
      --   Each feature is hardcoded to have an additional 'undefined/indeterminate'
      --   output, that is used in case the feature does not occur.
      cardinalities :: [Int],
      netSpec :: [LayerSpec]
    }

type WidgetLabel = [Label]

-- | A data set defines a set of samples for some game
data Dataset =
  Dataset
    { -- ^ Absolute paths to the images in the data set
      rootDir :: FilePath,
      -- ^ The labels to extract from an image. The order and length
      --   of the labels should be the same as the total number of
      --   feature positions for the given game. Paths are absolute.
      labels :: FilePath -> [[WidgetLabel]],
      -- ^ The rectangle to crop the images to. This should be the
      --   largest possible area that only captures the game screen.
      cropRect :: Rect Int,
      -- ^ Indicates the number of extra pixels we can crop off
      --  in all directions. This is used to apply a random
      --  translation to the image.
      wiggle :: Int,
      -- ^ Whether or not to apply random color distortion to the
      --   sample images
      distort :: Bool
    }
