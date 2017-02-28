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

type Game = Melee
type BatchSize = 15

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
  do v :: Visor Game <- loadVisor
     error "to do"

main' ["label", path] =
  do v :: Visor Game <- loadVisor
     shot <- readShot (Path path)
     y <- feedImage shot v
     putStrLn$ "Label: " ++ show y
     putStrLn$ "Interpretation: " ++ show (delabel y :: Game)

main' ["train"] =
  do v :: Visor Game <- loadVisor
     Just v' <- runConduitRes$ datasetSampleSource False .| trainC v .| takeC 10 .| lastC
     saveVisor v'

main' ["makebatch"] =
  do clear "batch"
     runConduitRes$ datasetSampleSource True
                 .| (batchify :: BatchC BatchSize Game)
                 .| (saveMany "batch")

main' ["trainbatch"] =
  do v :: Visor Game <- loadVisor
     Just v' <- runConduitRes$ forever ((loadMany "batch" :: RTSource (BatchVec BatchSize Game)) .| takeC 1)
                            .| trainBatchC v
                            .| takeC 100
                            .| lastC
     saveVisor v'

main' l = putStrLn$ "Unrecognized argument list: " ++ unwords l
