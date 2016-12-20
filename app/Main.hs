{-# LANGUAGE ViewPatterns #-}

module Main where

import Cifar
import Conduits
import Visor
import Screen
import Games.Melee
import System.Environment

main :: IO ()
main = getArgs >>= main'

main' :: [String] -> IO ()

main' ["gen", read -> n] =
  genBatch n dolphin_sets melee

main' ["datasetfeatures", read -> ndrop, read -> n] =
  runConduitRes $ datasetSource dolphin_sets
               .| (dropC ndrop >> takeC n)
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

main' ["train", read -> n] =
  do Just vIn <- genVisor melee
     Just vOut <- runConduitRes $ loopC (batchSource "SSBM")
                               .| awaitForever (replicateC 20)
                               .| trainC vIn
                               .| takeC n
                               .| lastC

     runConduitRes $ yield vOut .| visorSink
     return ()

main' ["croptest", read -> x, read -> y, read -> w, read -> h] =
  do runConduitRes $ screenSource x y w h
         .| takeC 1
         .| awaitForever (liftIO . undefined)
     return ()

main' ["cifar"] = 
  do w <- runConduitRes $ loopC sourceCifar

                       .| takeC 600000
                       .| train3C cifarNet
     return ()

main' _ = putStrLn "No valid command line argument given"
