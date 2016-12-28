module Games.Melee where

import Game
import ConvNet
import Util
import Label
import System.FilePath.Posix
import Data.List.Split

-- | Game definition for SSBM.
melee :: Game
melee = Game { title   = "SSBM"
             , widgets = [dmgStocks]
             }

dmgStocks :: Widget
dmgStocks = Widget { resolution = 32
                   , position = [(w 36, h 762), (w 336, h 762)]
                   , dimensions = (w 300, h 260)
                   , cardinalities = [10, 10, 10, 5]
                   , netSpec = [ConvS 13 64, ReLUS, PoolS, ConvS 5 64, ReLUS, PoolS]
                   }

-- Time is not yet included. It adds little important information, and is pretty inefficient
-- until support for non-square inputs
time :: Widget
time = Widget { resolution = 40
              , position = [(w 482, h 42)]
              , dimensions = (w 262, h 262)
              , cardinalities = [10,10,10]
              , netSpec = [ConvS 13 16, ReLUS, PoolS, ConvS 5 16, ReLUS, PoolS]
              }

screenWidth, screenHeight :: Double
screenWidth = 1252
screenHeight = 1028
w, h :: Double -> Double
w x = x / screenWidth
h y = y / screenHeight

ssbm_root :: FilePath
ssbm_root = "/Users/jmc/tmp/"

filterHidden :: [FilePath] -> [FilePath]
filterHidden = filter ((/= '.') . head)

prepend :: FilePath -> [FilePath] -> [FilePath]
prepend fBase = fmap (fBase</>)

-- | Gives the digits in cs in order of increasing significance
--   "123" -> [Just 3, Just 2, Just 1, Nothing, Nothing..
asDigits :: String -> [Label]
asDigits cs = fmap readDigit (reverse cs) ++ repeat Indeterminate

dolphin_sets :: Dataset
dolphin_sets =
  Dataset { rootDir = "/Users/jmc/tmp/"
          , labels = \f ->
            let ["shot",_,stage,framesPassed,p1p,p1s,p2p,p2s] = splitOn "_" (takeBaseName f)
                ingame = stage /= "0"
                percentLegible p = take 3 $ if ingame && p /= "X" then asDigits p else repeat Indeterminate
                p1p' = if p1s /= "0" then percentLegible p1p else replicate 3 Indeterminate
                p2p' = if p2s /= "0" then percentLegible p2p else replicate 3 Indeterminate
                p1Stocks = if ingame then Label (read p1s) else Label 0
                p2Stocks = if ingame then Label (read p2s) else Label 0
                matchLength = 8 * 60 * 60
                time
                  | not ingame = replicate 3 Indeterminate
                  | otherwise  = let frames = matchLength - read framesPassed
                                     secsTotal = frames `div` 60
                                     (mins, secs) = secsTotal `divMod` 60
                                     (secs10, secs1) = secs `divMod` 10
                                  in Label <$> [mins, secs10, secs1]

             in [ [p1p' ++ [p1Stocks], p2p' ++ [p2Stocks]]
                , [time]
                ]
          , cropRect = Rect 335 50 1251 1027
          , wiggle = 30
          , distort = True
          }

data GameState = Menu
               | Ingame { p1p :: Int
                        , p1s :: Int
                        , p2p :: Int
                        , p2s :: Int
                        } deriving Eq
