{-# LANGUAGE ScopedTypeVariables #-}

module Images where

import Util
import Data.Array.Repa hiding ((++))
import Codec.Picture
import Codec.Picture.Extra
import Data.Word
import System.Random

type FullImage a = Int

{-
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
         labels = parseFn fp
     return $! LabeledImage (imgFinal, labels)

-}
