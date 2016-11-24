module Main where

import System.Environment
import Control.Monad
import Network
import Batch
import Game
import Games.Melee

main :: IO ()
main = genBatches melee

main' :: [String] -> IO ()

main' ["spiral"] =
  do s <- spiral 100 3
     n <- initNet 2 3 [100, 100] 1e-3 1
     trainNtimes s n 10000

main' _ = putStrLn "Main"

tla :: Network -> Batch -> (Network, Double, Double)
tla n b = (train n b, loss n b, accuracy n b)

testNetwork :: Batch -> Network -> IO ()
testNetwork b n =
  do let n'  = train n b
         l   = loss n b
         acc = accuracy n b
     print l
     unless (acc > 0.95) $ testNetwork b n'

trainNtimes :: Batch -> Network -> Int -> IO ()
trainNtimes b net n = do let (net', l, a) = tla net b
                         putStrLn $ show n ++ ": " ++ show l ++ "  " ++ show a
                         if n == 0 then print $ feed net (input b)
                                   else trainNtimes b net' (n-1)
