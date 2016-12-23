module Images where

import Game
import Volume
import ConvNet
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
toVolume img = computeS $ fromFunction sh fn
  where
    w = imageWidth img
    h = imageHeight img
    sh = Z:.3:.h:.w
    fn (Z:.0:.y:.x) = let PixelRGB8 r _ _ = pixelAt img x y in fromIntegral r / 255
    fn (Z:.1:.y:.x) = let PixelRGB8 _ g _ = pixelAt img x y in fromIntegral g / 255
    fn (Z:.2:.y:.x) = let PixelRGB8 _ _ b = pixelAt img x y in fromIntegral b / 255
    fn _ = undefined

-- | Turns an image and its labels into a list of lists of samples
--   The elements of the outer list each associate with a different
--   widget, the inner lists are different occurrences of single widget.
toSamples :: Game -> (Palette, [[WidgetLabel]]) -> [[ConvSample]]
toSamples game ins = (fmap.fmap) (\ (img, ls) -> ConvSample (toVolume img) ls) extracted
  where
    extracted :: [[(Palette, WidgetLabel)]]
    extracted = extractWidgets game ins
