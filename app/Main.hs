{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Types
import Visor
import Lib
import IO
import Games.Melee
import System.Environment
import Control.Monad
import Conduit
import System.FilePath

type Game = Melee
type BatchSize = 16

main :: IO ()
main = getArgs >>= main'

main' :: [String] -> IO ()
main' ["parse", path] = print l
  where l = parse (Path path :: Path Game)

main' ["ls"] =
  do paths :: [Path Game] <- runConduitRes$ pathSource .| sinkList
     putStrLn.unlines$ show <$> paths
     putStrLn$ show (length paths) ++ " images in dataset"

main' ["lsparse"] =
  do paths :: [Path Game] <- runConduitRes$ pathSource .| sinkList
     putStrLn.unlines$ showAndLabel <$> paths
     putStrLn$ show (length paths) ++ " images in dataset"

main' ["label"] =
  do v <- loadVisor
     runConduitRes$ pathSource .| mapMC (\p -> liftIO $ f v p) .| sinkNull
       where
         f :: Visor Game -> Path Game -> IO ()
         f v p = do fx <- readShot p >>= feedImage v
                    if fx == parse p
                       then putStrLn "-"
                       else do print p
                               putStrLn$ "Expected: " ++ show (parse p)
                               putStrLn$ "Actual:   " ++ show fx

main' ["label", path] =
  do v :: Visor Game <- loadVisor
     y <- readShot (Path path) >>= feedImage v
     putStrLn$ "Label: " ++ show y
     putStrLn$ "Interpretation: " ++ show (delabel y :: Game)

main' ["crops", path] =
  do crops :: InputVec Game <- readShot (Path path :: Path Game) >>= extract
     clear "crops"
     dumpCrops 0 (dir</>"crops") crops

main' ["train"] =
  do v :: Visor Game <- loadVisor
     Just v' <- runConduitRes$ forever (datasetSampleSource True) .| trainC v .| takeC 10000 .| lastC
     saveVisor v'

main' ["makebatch"] =
  do clear "batch"
     runConduitRes$ datasetSampleSource True
                 .| (batchify :: BatchC BatchSize Game)
                 .| takeC 1
                 .| (saveMany "batch")

main' ["trainbatch"] =
  do v :: Visor Game <- loadVisor
     Just v' <- runConduitRes$ forever (loadMany "batch" :: RTSource (BatchVec BatchSize Game))
                            .| trainBatchC v
                            .| takeC 100
                            .| lastC
     saveVisor v'

main' ["kernels"] =
  do v :: Visor Game <- loadVisor
     saveKernels v

main' l = putStrLn$ "Unrecognized argument list: " ++ unwords l
