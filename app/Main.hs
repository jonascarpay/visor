{-# LANGUAGE ViewPatterns #-}

module Main where

import Cifar
import Conduits
import Data.Conduit.Async
import Games.Melee
import System.Environment

main :: IO ()
main = getArgs >>= main'

main' :: [String] -> IO ()

main' ["cifar"] =
  do net <- runResourceT $ buffer 1 (loopC sourceCifar .| takeC 600) (train3C cifarNet)
     saveWeightImages net

main' _ = putStrLn "No valid command line argument given"
