{-# LANGUAGE ScopedTypeVariables #-}

module Screen where

import Conduit
import Util
import Codec.Picture
import Codec.Picture.Extra
import System.Directory
import System.Process
import Data.Word
import Data.Array.Repa hiding ((++))
import Data.Array.Repa.IO.BMP
import Volume

screenSource :: Int -> Int -> Int -> Int -> IOSrc Palette
screenSource x y w h = do img <- liftIO$ do _ <- system "screencapture -xm /Users/jmc/Desktop/out.png"
                                            Right dimg <- readImage "/Users/jmc/Desktop/out.png"
                                            removeFile "/Users/jmc/Desktop/out.png"
                                            return dimg
                          yield $ crop x y w h (convertRGB8 img)
                          screenSource x y w h

screenSourceRepa :: Int -> Int -> Int -> Int -> IOSrc (Array U DIM2 (Word8, Word8, Word8))
screenSourceRepa x y w h = do img <- liftIO$ do let string = "screencapture -xm -R" ++ show x ++ ',':show y ++ ',':show w ++ ',':show h ++ " -t bmp /Users/jmc/Desktop/out.bmp"
                                                _ <- system string
                                                Right img <- readImageFromBMP "/Users/jmc/Desktop/out.bmp"
                                                removeFile "/Users/jmc/Desktop/out.bmp"
                                                return img
                              yield img
                              screenSourceRepa x y w h
