{-# LANGUAGE ViewPatterns #-}

module Main where

import Cifar
import Visor
import Conduits
import Data.Conduit.Async
import Games.Melee
import Data.Serialize
import System.Environment
import System.FilePath
import System.Directory
import qualified Data.ByteString as BS

main :: IO ()
main = getArgs >>= main'

main' :: [String] -> IO ()

main' ["datasetTest"] = runResourceT $ buffer 1 (datasetSource False dolphin_sets) datasetSink

main' ["parseTest"] = runResourceT $ buffer 1 (datasetSource False dolphin_sets) (parseSink melee)

-- Trains the initial visor directly from the dataset, without intermediate batches
main' ["meleeRaw"] =
  do Visor net <- runResourceT $ buffer 1
                                   (loopC (gameSource melee dolphin_sets False) .| takeC 10000)
                                   (trainVisorC (gameVisor melee))
     saveWeightImages (head net)

main' ["melee", n] =
  do let dir = "data"</>"visor"
         vFile = dir</>"melee.visor"
     --exist <- doesFileExist vFile
     Visor net <- runResourceT $ buffer 1
                                   ( if n == "all" then batchSource
                                                   else loopC batchSource .| takeC (read n))
                                   (trainVisorC (gameVisor melee))
     saveWeightImages (head net)
     createDirectoryIfMissing True dir
     BS.writeFile vFile (encode (Visor net))

main' ["genBatch", read->n] =
  runResourceT $ buffer n (gameSource melee dolphin_sets True) (batchSink n)

-- Test training on CIFAR-10 dataset
main' ["cifar"] =
  do net <- runResourceT $ buffer 1 (loopC sourceCifar .| takeC 600) (trainC cifarNet)
     saveWeightImages net

main' ["processCifar"] = runConduitRes $ sourceCifar .| imageSink

main' _ = putStrLn "No valid command line argument given"
