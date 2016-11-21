module Main where

import System.Environment
import Control.Monad
import Network
import Batch

main :: IO ()
main = getArgs >>= main'

main' :: [String] -> IO ()
main' ["spiral"] =
  do s <- spiral 100 3
     n <- initNet 2 3 [100] 1e-3 1
     testNetwork s n
main' _          = putStrLn "Main"

testNetwork :: Batch -> Network -> IO ()
testNetwork b n =
  do let n'  = train n b
         l   = loss n b
         acc = accuracy n b
     print l
     unless (acc > 0.8) $ testNetwork b n'
