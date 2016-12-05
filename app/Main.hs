module Main where

import Conduits
import Batch
import qualified Data.Conduit.Combinators as CC
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
                          .| lastDefC undefined
             return ()

foldl1' f = do Just x <- CC.foldl1 f
               return x
