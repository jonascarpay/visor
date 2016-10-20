{-# LANGUAGE TypeOperators #-}

module Main where

import Prelude hiding (words)
import Data.Word
import Data.Array.Repa as R
import Data.Array.Repa.Repr.ForeignPtr
import Data.Array.Repa.IO.DevIL
import Control.Monad (when)
import System.Directory

input, output :: String
input  = "assets/groot.png"
output = "assets/output.png"

crop :: Int -> Int -> Int -> Int -> Array F DIM3 Word8 -> Array D DIM3 Word8
crop x1 y1 x2 y2 img = extract (Z:.h-y2:.x1:.d) (Z:.y2-y1:.x2-x1:.d) img
  where (Z:.h:._:.d) = extent img

toGrey :: Array F DIM3 Word8 -> Array D DIM2 Int
toGrey a = R.traverse a flat grey
  where flat (sh:._) = sh

        grey :: (DIM3 -> Word8) -> DIM2 -> Int
        grey f sh = ceiling (0.21*r + 0.71*g + 0.07*b :: Double)
          where
            r = fromIntegral $ f (sh:.0)
            g = fromIntegral $ f (sh:.1)
            b = fromIntegral $ f (sh:.2)

toRgba :: Array D DIM2 Int -> Array D DIM3 Word8
toRgba a = R.traverse a expand rgba
  where expand sh = sh:.4 :: DIM3
        rgba _ (_ :.3) = 255
        rgba f (sh:._) = fromIntegral . f $ sh


delete :: FilePath -> IO ()
delete f = do exists <- doesFileExist f
              when exists $ removeFile f

main :: IO ()
main = do
  delete output
  runIL $ do
    (RGBA i) <- readImage input
    ts <- computeP $ toRgba . toGrey $ i
    writeImage output (RGBA ts)
