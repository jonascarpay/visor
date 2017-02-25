{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Types
import Visor
import Games.Melee
import System.Environment
import Conduit
import IO

type Game = Melee

set :: Dataset Melee
set = dataset

main :: IO ()
main = getArgs >>= main'

main' :: [String] -> IO ()
main' ["parse", path] = print l
  where l = parseFilename set path

main' ["ls"] =
  do paths <- runConduitRes$ datasetPathSource set .| sinkList
     putStrLn$ unlines paths
     putStrLn$ show (length paths) ++ " images in dataset"

main' ["label", path] =
  do v <- (loadVisor :: IO (Visor Game))
     shot <- readShot path
     y <- feedImage shot v
     putStrLn$ "Label: " ++ show y
     putStrLn$ "Interpretation: " ++ show (delabel y :: Game)

main' l = putStrLn$ "Unrecognized argument list: " ++ unwords l
