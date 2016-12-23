module Main where

import VolumeSpec
import Control.Monad


main :: IO ()
main = void runtests

maintest :: IO ()
maintest = do _ <- runtests
              return ()
