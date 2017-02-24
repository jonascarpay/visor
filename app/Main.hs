{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Types
import Games.Melee
import System.Environment

type Game = Melee

main :: IO ()
main = getArgs >>= main'

main' :: [String] -> IO ()
main' ["parse", path] = print l
  where l = parseFilename (dataset :: Dataset Game) path

main' l = putStrLn$ "Unrecognized argument list: " ++ unwords l
