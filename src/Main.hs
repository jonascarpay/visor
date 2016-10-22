{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}

module Main where

import Prelude hiding (words)
import Data.Word
import Data.Array.Repa as R
import Data.Array.Repa.Stencil.Dim2
import Data.Array.Repa.Stencil
import Data.Array.Repa.Repr.ForeignPtr
import Data.Array.Repa.IO.DevIL hiding (Image)
import Control.Monad (when)
import System.Directory
import System.Environment
import Data.Monoid

type Image r  = Array r DIM2 Float

-- Gaussian blur
blurred :: Source r1 Float => Image r1 -> Image D
blurred = R.map (/256) . blurSepX . blurSepY

blurSepX :: Source r1 Float => Image r1 -> Image PC5
blurSepX = mapStencil2 BoundClamp [stencil2| 1 4 6 4 1 |]

blurSepY :: Source r1 Float => Image r1 -> Image PC5
blurSepY = mapStencil2 BoundClamp [stencil2| 1
                                             4
                                             6
                                             4
                                             1 |]

-- Greyscaling
toGrey :: Array F DIM3 Word8 -> Image D
toGrey a = R.traverse a flat grey
  where flat (sh:._) = sh

        grey :: (DIM3 -> Word8) -> DIM2 -> Float
        grey f sh = 0.21*r + 0.71*g + 0.07*b
          where
            r = fromIntegral $ f (sh:.0)
            g = fromIntegral $ f (sh:.1)
            b = fromIntegral $ f (sh:.2)

toRgba :: Source r Float => Image r -> Array D DIM3 Word8
toRgba a = R.traverse a expand rgba
  where expand sh = sh:.4 :: DIM3
        rgba _ (_ :.3) = 255
        rgba f (sh:._) = truncate . f $ sh

-- Cropping
crop :: Int -> Int -> Int -> Int -> Array F DIM3 Word8 -> Array D DIM3 Word8
crop x1 y1 x2 y2 img = extract (Z:.h-y2:.x1:.d) (Z:.y2-y1:.x2-x1:.d) img
  where (Z:.h:._:.d) = extent img

-- Misc
delete :: FilePath -> IO ()
delete f = do exists <- doesFileExist f
              when exists $ removeFile f

main :: IO ()
main = do
  [file] <- getArgs
  let input  = "assets/"   <> file
      output = "assets/p_" <> file
  delete output
  runIL $ do
    (RGBA i) <- readImage input
    ts <- computeP $ toRgba . blurred . toGrey $ i
    writeImage output (RGBA ts)
