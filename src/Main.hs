{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Prelude hiding (words)
import Data.Word
import qualified Data.Array.Repa as R
import Data.Array.Repa.IO.DevIL hiding (Image)
import Data.Array.Accelerate.IO
import Data.Array.Accelerate
import Data.Array.Accelerate.CUDA
import Control.Monad (when)
import System.Directory
import System.Environment
import Data.Monoid
import ImageProcessing

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
    accIn <- fromRepa <$> R.copyP i
    let accOut = run (unpackGrey . packGrey $ lift accIn)
    o <- R.copyP $ toRepa accOut
    writeImage output (RGB o)
