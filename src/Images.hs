{-# LANGUAGE ScopedTypeVariables #-}

module Images where

import Game
import Volume
import ConvNet
import Util
import Data.Array.Repa hiding ((++))
import Codec.Picture
import Codec.Picture.Extra
import Data.Word
import System.Random

newtype LabeledWidgets a = LabeledWidgets { getWidgets :: [[(Palette, WidgetLabel)]] }
newtype LabeledImage   a = LabeledImage (Palette, [[WidgetLabel]])

newtype FullImage   a = UnlabeledImage Palette
newtype WidgetImage a = UnlabeledWidget [[Palette]]

loadImage :: forall a. GameState a => Dataset a -> FilePath -> IO (LabeledImage a)
loadImage (Dataset _ parseFn mRect wig dist) fp =
  do putStrLn $ "Loading " ++ fp
     Right img' <- readImage fp
     dx <- randomRIO (0, wig)
     dy <- randomRIO (0, wig)
     dw <- randomRIO (0, wig)
     dh <- randomRIO (0, wig)
     dr :: Double <- randomRIO (0.9, 1.1)
     dg :: Double <- randomRIO (0.9, 1.1)
     db :: Double <- randomRIO (0.9, 1.1)
     let img = convertRGB8 img'
         imgCropped = case mRect of
                        Nothing             -> img
                        Just (Rect x y w h) -> crop (x+dx) (y+dy) (w-dx-dw) (h-dy-dh) img
         scaleMax x c = round $ min 255 $ fromIntegral x * c
         distortColor (PixelRGB8 r g b) = PixelRGB8 (scaleMax r dr) (scaleMax g dg) (scaleMax b db)
         distorted = pixelMap distortColor imgCropped
         imgFinal = if dist then distorted else imgCropped
         labels = toLabel (parseFn fp :: a)

     return $! LabeledImage (imgFinal, labels)

extractWidgetsLabeled :: GameState a => LabeledImage a -> LabeledWidgets a
extractWidgetsLabeled i@(LabeledImage (img, labels)) = LabeledWidgets $ pair imgs labels
  where pair = Prelude.zipWith (zip)
        imgs = extractWidgets widgets img
        widgets = widgetDfn . config $ i

extractWidgets :: [Widget a] -> Palette -> [[Palette]]
extractWidgets ws img =
  let w = imageWidth img
      h = imageHeight img
      w' x = round $ x * fromIntegral w
      h' x = round $ x * fromIntegral h
      getWidgets (Widget r ps (rw, rh) _ _) =
        fmap (\(rx, ry) -> scaleBilinear r r $ crop (w' rx) (h' ry) (w' rw) (h' rh) img) ps
   in fmap getWidgets ws

cropScale :: (GameState a, Monad m) => p a -> Array U DIM2 (Word8, Word8, Word8) -> m [[Volume]]
cropScale proxy img = mapM getWidget ws
  where
    Z:.h:.w = extent img
    xAbs x = round$ x * fromIntegral w
    ws = widgetDfn . config $ proxy
    yAbs y = round$ y * fromIntegral h
    getWidget (Widget r ps (rw, rh) _ _) = mapM (\(rx, ry) -> cropFast r (xAbs rx) (yAbs ry) (xAbs rw) (yAbs rh) img) ps

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

toVolumeP :: Monad m => Palette -> m Volume
toVolumeP img = computeP $ fromFunction sh fn
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
toSamples :: GameState a => LabeledImage a -> [[ConvSample]]
toSamples i = (fmap.fmap) (\ (img, ls) -> ConvSample (toVolume img) ls) extracted
  where
    extracted :: [[(Palette, WidgetLabel)]]
    extracted = getWidgets $ extractWidgetsLabeled i

-- TODO: Enforce 0-1 normalization?
rgbToImage :: Volume -> DynamicImage
rgbToImage vol = ImageRGB8 $ generateImage convFn w h
  where
    Z:.3:.h:.w = extent vol
    c x = round $ x * 255
    convFn x y = let r = vol ! ix3 0 y x
                     g = vol ! ix3 1 y x
                     b = vol ! ix3 2 y x
                  in PixelRGB8 (c r) (c g) (c b)

greyScaleToImage :: Monad m => Matrix -> m DynamicImage
greyScaleToImage img = do img' <- lerp img 0 255
                          return . ImageRGB8 $ generateImage (arrLkUp img') w h
  where
    Z:.h:.w = extent img
    arrLkUp a x y = let v = a ! ix2 y x
                     in PixelRGB8 (round v) (round v) (round v)

rgbNormalizeToImage :: Monad m => Volume -> m DynamicImage
rgbNormalizeToImage img = do img' <- lerp img 0 255
                             return . ImageRGB8 $ generateImage (arrLkUp img') w h
  where
    Z:.3:.h:.w = extent img
    arrLkUp a x y = let r = a ! ix3 0 y x
                        g = a ! ix3 1 y x
                        b = a ! ix3 2 y x
                     in PixelRGB8 (round r) (round g) (round b)
