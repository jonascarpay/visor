{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DataKinds #-}

module Lib where

import Types
import Network.Label
import Text.Read as T
import System.FilePath.Posix
import Numeric
import Data.List

divs :: Integral a => a -> a -> Bool
a `divs` b = b `mod` a == 0

count :: (a -> Bool) -> [a] -> Int
count p = length . filter p

splitDigits :: Int -> (Int, Int, Int)
splitDigits x = (d100, d10, d1)
  where (d100, r) = x `divMod` 100
        (d10, d1) = r `divMod` 10

toWidget :: Widget a => WLabel a -> a
toWidget (WLabel l) = case parseLabel l fromLabel of
                        Left str -> error str
                        Right a -> a

read' :: String -> Int
read' x = case T.readMaybe x of
           Just x -> x
           Nothing -> error$ "error parsing " ++ x

labelPath :: GameState g => Path g -> g
labelPath = delabel . parse

pmap :: (FilePath -> FilePath) -> Path a -> Path a
pmap f (Path p) = Path$ f p

showAndLabel :: GameState g => Path g -> String
showAndLabel p = show (pmap takeBaseName p) ++ "\t\t" ++ show (labelPath p)

showE :: Double -> ShowS
showE = showEFloat (Just 4)

median :: Ord a => [a] -> a
median [] = undefined
median [x] = x
median xs = let i = length xs `div` 2
             in sort xs !! i

median' :: [Double] -> Double
median' [] = undefined
median' [x] = x
median' xs = let l = length xs
                 i = l `div` 2
                 s = sort xs
              in if odd l
                    then s !! i
                    else (s !! i + s !! (i-1)) / 2

roundF x = fromInteger (round   x :: Integer)
ceilF  x = fromInteger (ceiling x :: Integer)
floorF x = fromInteger (floor   x :: Integer)
