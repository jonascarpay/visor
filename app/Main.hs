{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import System.Environment
import Games.Melee

type Game = Melee

main :: IO ()
main = getArgs >>= main'

main' :: [String] -> IO ()
main' ["label", path] = undefined
