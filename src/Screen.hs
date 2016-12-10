{-# LANGUAGE ScopedTypeVariables #-}

module Screen where

import Conduit
import System.Process
import Util
import Vision.Image
import Vision.Image.Storage.DevIL
import Vision.Primitive

screenSource :: Int -> Int -> Int -> Int -> IOSrc RGBDelayed
screenSource x y w h = do Right (img :: RGB) <- liftIO $ do callCommand "screencapture -xm /Users/jmc/Desktop/out.png"
                                                            load PNG "/Users/jmc/Desktop/out.png"
                          yield $ crop (Rect x y w h) img
                          screenSource x y w h
