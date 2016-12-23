{-# LANGUAGE ViewPatterns #-}

module Main where

import Cifar
import Visor
import Conduits
import Data.Conduit.Async
import Games.Melee
import System.Environment

main :: IO ()
main = getArgs >>= main'

main' :: [String] -> IO ()

main' ["datasetTest"] = runResourceT $ buffer 1 (datasetSource False dolphin_sets) datasetSink

main' ["parseTest"] = runResourceT $ buffer 1 (datasetSource False dolphin_sets) (parseSink melee)

-- Trains the initial visor directly from the dataset, without intermediate batches
main' ["meleeRaw"] =
  do Visor net <- runResourceT $ buffer 1
                                   (loopC (gameSource melee dolphin_sets False) .| takeC 6000)
                                   (trainVisorC (gameVisor melee))
     saveWeightImages (head net)

main' ["genBatch", read->n] =
  runConduitRes $ gameSource melee dolphin_sets True
               .| batchSink n

-- Test training on CIFAR-10 dataset
main' ["cifar"] =
  do net <- runResourceT $ buffer 1 (loopC sourceCifar .| takeC 600) (trainC cifarNet)
     saveWeightImages net

main' ["processCifar"] = runConduitRes $ sourceCifar .| imageSink

main' _ = putStrLn "No valid command line argument given"
