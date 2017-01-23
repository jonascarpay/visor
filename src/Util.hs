{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DataKinds #-}

module Util where

import Types
import Conduit
import Data.List

type IOSrc     a   = Source    (ResourceT IO) a
type IOConduit a b = Conduit a (ResourceT IO) b
type IOSink    a   = Sink    a (ResourceT IO) ()

relToAbs :: Int -> Int -> Rect Double -> Rect Int
relToAbs (fromIntegral -> w) (fromIntegral -> h) (Rect rx ry rw rh)
  = Rect (round $ w*rx) (round $ h*ry) (round $ w*rw) (round $ h*rh)

divs :: Integral a => a -> a -> Bool
a `divs` b = b `mod` a == 0

printDoubleThresholded :: Double -> Double -> String
printDoubleThresholded t x = (if x < t then green else red) ++ show x ++ reset
  where green = "\x1b[0m"
        red   = "\x1b[94m"
        reset = "\x1b[0m"

printLosses :: [[[Double]]] -> String
printLosses dsss = dsss >>= (intercalate "\n" . fmap f)
  where f = intercalate "\t" . fmap (printDoubleThresholded 0.7)

count :: Eq a => a -> [a] -> Int
count needle = length . filter (==needle)

digit :: Int -> Label 10
digit x | x >= 0 && x <= 9 = Label x
        | otherwise = undefined
