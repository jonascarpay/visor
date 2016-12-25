{-# LANGUAGE ScopedTypeVariables #-}

module Screen where

import Conduit
import Util
import Codec.Picture
import Codec.Picture.Extra
import System.Directory
import System.Process

screenSource :: Int -> Int -> Int -> Int -> IOSrc Palette
screenSource x y w h = do img <- liftIO$ do _ <- system "screencapture -xm /Users/jmc/Desktop/out.png"
                                            Right dimg <- readImage "/Users/jmc/Desktop/out.png"
                                            removeFile "/Users/jmc/Desktop/out.png"
                                            return dimg
                          yield $ crop x y w h (convertRGB8 img)
                          screenSource x y w h
