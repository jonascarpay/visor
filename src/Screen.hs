{-# LANGUAGE ScopedTypeVariables #-}

module Screen where

import Conduit
import System.Process
import Util

screenSource :: Int -> Int -> Int -> Int -> IOSrc RGBDelayed
screenSource x y w h = do Right (img :: RGB) <- liftIO $ do _ <- system "screencapture -xm /Users/jmc/Desktop/out.png"
                                                            undefined -- load PNG "/Users/jmc/Desktop/out.png"
                          yield $ undefined -- crop (Rect x y w h) img
                          screenSource x y w h
