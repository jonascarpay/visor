{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Game where

import Util
import Label
import ConvNet

data GameConfig a = GameConfig { defaultParams :: NetParams
                               , title         :: String
                               , widgetDfn     :: [Widget a]
                               }

-- | If x1 and x2 are values for some x in two subsequent ingame
--   frames, then x1 `canTransition` x2
class Transitions a where
  canTransition :: a -> a -> Bool

class Show a => GameState a where
  fromLabel     :: [[WidgetLabel]] -> a
  toLabel       :: a -> [[WidgetLabel]]
  validate      :: a -> a -> Bool
  config        :: p a -> GameConfig a
  showFancy     :: a -> String

-- | A widget represents a (possibly repeating) rectangular area of
--   the screen containing information to be extracted.
data Widget a =
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

-- | A data set defines a set of samples for some game
data Dataset a =
  Dataset
    { -- ^ Absolute paths to the images in the data set
      rootDir :: FilePath,
      parseFilename :: FilePath -> [[WidgetLabel]],
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
