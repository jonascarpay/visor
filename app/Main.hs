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

main' ["datasetTest"] = runResourceT $ buffer 1 (datasetSource dolphin_sets) datasetSink

main' ["parseTest"] = runResourceT $ buffer 1 (datasetSource dolphin_sets) (parseSink melee)

main' ["cifar"] =
  do net <- runResourceT $ buffer 1 (loopC sourceCifar .| takeC 600) (trainC cifarNet)
     saveWeightImages net

main' ["processCifar"] = runConduitRes $ sourceCifar .| imageSink

main' ["trainMelee"] = runResourceT $ buffer 1 (datasetSource dolphin_sets) undefined

main' _ = putStrLn "No valid command line argument given"
