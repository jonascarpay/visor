{-# LANGUAGE ViewPatterns #-}

module Util where

import Conduit
import Label
import Data.List

readDigit :: Char -> Label
readDigit '1' = Label 1
readDigit '2' = Label 2
readDigit '3' = Label 3
readDigit '4' = Label 4
readDigit '5' = Label 5
readDigit '6' = Label 6
readDigit '7' = Label 7
readDigit '8' = Label 8
readDigit '9' = Label 9
readDigit '0' = Label 0
readDigit _   = Indeterminate

type IOSrc a       = Source (ResourceT IO) a
type IOConduit a b = Conduit a (ResourceT IO) b
type IOSink a      = Sink a (ResourceT IO) ()

data Rect a = Rect { rx :: a
                   , ry :: a
                   , rw :: a
                   , rh :: a }

relToAbs :: Int -> Int -> Rect Double -> Rect Int
relToAbs (fromIntegral -> w) (fromIntegral -> h) (Rect rx ry rw rh)
  = Rect (round $ w*rx) (round $ h*ry) (round $ w*rw) (round $ h*rh)

printDoubleThresholded :: Double -> Double -> String
printDoubleThresholded t x = (if x < t then green else red) ++ show x ++ reset
  where green = "\x1b[0m"
        red   = "\x1b[94m"
        reset = "\x1b[0m"

printLosses :: [[[Double]]] -> String
printLosses dsss = dsss >>= (intercalate "\n" . fmap f)
  where f = intercalate "\t" . fmap (printDoubleThresholded 0.7)

