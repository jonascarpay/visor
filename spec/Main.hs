module Main where

import VolumeSpec


main :: IO ()
main = putStrLn "Hello World"

maintest :: IO ()
maintest = do _ <- runtests
              return ()
