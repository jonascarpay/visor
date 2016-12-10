{-# LANGUAGE ViewPatterns #-}

module Main where

import Conduits
import Batch
import Visor
import Screen
import Games.Melee
import System.Environment

main :: IO ()
main = getArgs >>= main'

main' :: [String] -> IO ()

main' ["gen"] =
  genBatch 1024 dolphin_sets melee

main' ["features", read -> n] =
  runConduitRes $ datasetSource dolphin_sets
               .| takeC n
               .| featureSink melee

main' ["watchfeatures", read -> x, read -> y, read -> w, read -> h] =
  do Just meleeVisor <- genVisor melee
     runConduitRes $ screenSource x y w h
                  .| mapC (\i -> (i, vFeed melee meleeVisor i))
                  .| featureSink melee
     return ()

main' ["watch", read -> x, read -> y, read -> w, read -> h] =
  do Just meleeVisor <- genVisor melee
     runConduitRes $ screenSource x y w h
                  .| mapC (\i -> (i, vFeed melee meleeVisor i))
                  .| mapM_C (liftIO . print . delabelMelee . snd)
     return ()

main' _ =
  do Just vIn <- genVisor melee
     Just vOut <- runConduitRes $ batchSource "SSBM"
                               .| (foldl1' (zipWith stack) >>= repeatC)
                               .| trainC vIn
                               .| takeC 4000
                               .| lastC

     runConduitRes $ yield vOut .| visorSink
     return ()
