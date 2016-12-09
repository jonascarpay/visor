module Main where

import Conduits
import Batch
import Games.Melee
import System.Environment

main :: IO ()
main = getArgs >>= main'

main' :: [String] -> IO ()
main' ["gen"] = genBatch 1024 dolphin_sets melee

main' ["features", n] = runConduitRes $ datasetSource dolphin_sets
                                     .| takeC (read n)
                                     .| featureSink melee

main' _ = do Just vIn <- genVisor melee
             Just vOut <- runConduitRes $ batchSource "SSBM"
                                       .| (foldl1' (zipWith stack) >>= repeatC)
                                       .| trainC vIn
                                       .| takeC 4000
                                       .| lastC

             runConduitRes $ yield vOut .| visorSink
             return ()
