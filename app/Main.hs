{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Cifar
import Visor
import Images
import Conduits
import Screen
import Data.Conduit.Async
import Games.Melee
import System.Environment
import System.FilePath

main :: IO ()
main = getArgs >>= main'

main' :: [String] -> IO ()

main' ["datasetTest"] = runResourceT $ buffer 1 (datasetSource False dolphinShots) datasetSink

main' ["parseTest"] = runResourceT $ buffer 1 ( datasetSource False dolphinShots
                                             .| mapC extractWidgetsLabeled
                                              )
                                              parseSink

main' ["batchParseTest"] = runResourceT $ buffer 1 (batchSource .| unpackBatch) parseSink

-- Trains the initial visor directly from the dataset, without intermediate batches
main' ["meleeRaw"] =
  do visor <- runResourceT $ buffer 1
                                   (loopC (gameSource dolphinShots False) .| takeC 10000)
                                   (trainVisorC (initVisor :: Visor Melee))
     saveWeightImages visor

main' ["meleeTrain", n] =
  do let vFile = "data"</>"visor"</>"melee.visor"
     let visor = initVisor :: Visor Melee -- Hardcoded to generate a new visor
     visor' <- runConduitRes (( if n == "all" then batchSource else loopC batchSource .| takeC (read n)) .| trainVisorC visor)
     saveWeightImages visor'
     saveVisor vFile visor'

main' ["meleeWatch", read -> x, read -> y, read -> w, read -> h] =
  do let vFile = "data"</>"visor"</>"melee.visor"
     (visor :: Visor Melee) <- loadVisor vFile
     runConduitRes $ screenSourceRepa x y w h
                  .| mapMC (cropScale (undefined :: p Melee))
                  .| mapMC (\img -> feedVisorFast visor img 0.99)
                  .| undefined

main' ["genBatch", read->n] =
  runResourceT $ buffer n (gameSource dolphinShots True) (batchSink n)

main' ["processCifar"] = runConduitRes $ sourceCifar .| imageSink

main' a = putStrLn $ "Invalid arguments: " ++ unwords a
