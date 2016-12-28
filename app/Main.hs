{-# LANGUAGE ViewPatterns #-}

module Main where

import Cifar
import Visor
import Images
import Conduits
import Screen
import Data.Conduit.Async
import Games.Melee
import Games.MeleeWatch
import System.Environment
import System.FilePath

main :: IO ()
main = getArgs >>= main'

main' :: [String] -> IO ()

main' ["datasetTest"] = runResourceT $ buffer 1 (datasetSource False dolphin_sets) datasetSink

main' ["parseTest"] = runResourceT $ buffer 1 ( datasetSource False dolphin_sets
                                             .| mapC (extractWidgetsLabeled melee)
                                              )
                                              parseSink

main' ["batchParseTest"] = runResourceT $ buffer 1 (batchSource .| unpackBatch) parseSink

-- Trains the initial visor directly from the dataset, without intermediate batches
main' ["meleeRaw"] =
  do visor <- runResourceT $ buffer 1
                                   (loopC (gameSource melee dolphin_sets False) .| takeC 10000)
                                   (trainVisorC (gameVisor melee))
     saveWeightImages visor

main' ["meleeTrain", n] =
  do let vFile = "data"</>"visor"</>"melee.visor"

     visor  <- loadVisor vFile melee
     visor' <- runResourceT $ buffer 1
                                   ( if n == "all" then batchSource
                                                   else loopC batchSource .| takeC (read n))
                                   (trainVisorC visor)
     saveWeightImages visor'
     saveVisor vFile visor'

main' ["meleeWatch", read -> x, read -> y, read -> w, read -> h] =
  do let vFile = "data"</>"visor"</>"melee.visor"
     visor <- loadVisor vFile undefined
     runConduitRes $ screenSourceRepa x y w h
                  .| mapMC (cropScale melee)
                  .| mapMC (\img -> feedVisorFast visor img 0.99)
                  .| meleeC

main' ["watchTest", read -> x, read -> y, read -> w, read -> h] =
  do let vFile = "data"</>"visor"</>"melee.visor"
     visor <- loadVisor vFile undefined
     runConduitRes $ screenSource x y w h
                  .| watchC visor melee 0.5
                  .| parseSink

main' ["genBatch", read->n] =
  runResourceT $ buffer n (gameSource melee dolphin_sets True) (batchSink n)

-- Test training on CIFAR-10 dataset
main' ["cifar"] =
  do net <- runResourceT $ buffer 1 (loopC sourceCifar .| takeC 600) (trainC cifarNet)
     saveWeightImages (Visor [net])

main' ["processCifar"] = runConduitRes $ sourceCifar .| imageSink

main' a = putStrLn $ "Invalid arguments: " ++ unwords a
