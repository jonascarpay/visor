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

main' ["datasetTest"] = runResourceT $ buffer 1 (datasetSource dolphin_sets) datasetSink

main' ["parseTest"] = runResourceT $ buffer 1 (datasetSource dolphin_sets) (parseSink melee)

main' ["cifar"] =
  do net <- runResourceT $ buffer 1 (loopC sourceCifar .| takeC 600) (trainC cifarNet)
     saveWeightImages net

main' ["melee"] =
  do Visor net <- runResourceT $ buffer 1
                                   (loopC (gameSource melee dolphin_sets) .| takeC 6000)
                                   (trainVisorC (gameVisor melee))
     saveWeightImages (head net)

main' ["processCifar"] = runConduitRes $ sourceCifar .| imageSink

main' _ = putStrLn "No valid command line argument given"
