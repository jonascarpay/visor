module Main where

import Data.Word
import Data.Array.Repa
import Data.Array.Repa.Repr.ForeignPtr
import Data.Array.Repa.IO.DevIL
import Control.Monad (when)
import System.Directory

input, output :: String
input  = "assets/test.png"
output = "assets/output.png"

delete :: FilePath -> IO ()
delete f = do exists <- doesFileExist f
              when exists $ removeFile f

crop :: Int -> Int -> Int -> Int -> Array F DIM3 Word8 -> Array D DIM3 Word8
crop x1 y1 x2 y2 img = cropped
  where (Z:.h:._:.d) = extent img
        cropped = extract (Z:.h-y2:.x1:.d) (Z:.y2-y1:.x2-x1:.d) img

main :: IO ()
main = do
  delete output
  runIL $ do
    (RGBA i) <- readImage input
    cropped <- computeP $ crop 0 0 1272 1064 i :: IL (Array F DIM3 Word8)
    writeImage output (RGBA cropped)
