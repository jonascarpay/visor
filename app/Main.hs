module Main where

import Conduits
import Batch
import Games.Melee
import System.Environment

main :: IO ()
main = getArgs >>= main'

main' :: [String] -> IO ()
main' ["gen"] = genBatch 256 dolphin_sets melee
main' _ = do Just v <- genVisor melee
             runConduitRes $ batchSource "SSBM"
                          .| (foldl1' (zipWith stack) >>= repeatC)
                          .| trainC v
                          .| takeC 1000
                          .| lastDefC undefined
             return ()
