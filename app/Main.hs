{-# LANGUAGE ViewPatterns #-}

module Main where

import Cifar
import Conduits
import Data.Conduit.Async
import Game
import Games.Melee
import System.Environment

main :: IO ()
main = getArgs >>= main'

main' :: [String] -> IO ()

main' ["setTest"] = runResourceT $ buffer 1 (datasetSource dolphin_sets) (datasetSink)

main' ["parseTest"] = runResourceT $ buffer 1 (datasetSource dolphin_sets) (parseSink melee)

main' ["cifar"] =
  do net <- runResourceT $ buffer 1 (loopC sourceCifar .| takeC 600) (train3C cifarNet)
     saveWeightImages net

main' _ = putStrLn "No valid command line argument given"
