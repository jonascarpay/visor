module Images where

import Game
import Volume
import Data.Array.Repa
import Codec.Picture
import Codec.Picture.Extra

extractWidgets :: Game -> (Palette, [[WidgetLabel]]) -> [[(Palette, WidgetLabel)]]
extractWidgets (Game _ ws) (img, ls) =
  let w = imageWidth img
      h = imageHeight img
      w' x = round $ x * fromIntegral w
      h' x = round $ x * fromIntegral h
      getWidgets (Widget r ps (rw, rh) _) =
        fmap (\(rx, ry) -> scaleBilinear r r $ crop (w' rx) (h' ry) (w' rw) (h' rh) img) ps
      pair = Prelude.zipWith zip
   in pair (fmap getWidgets ws) ls

toVolume :: Palette -> Volume
toVolume img = undefined
