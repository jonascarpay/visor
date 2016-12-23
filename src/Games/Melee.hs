module Games.Melee where

import Game
import ConvNet
import Util
import Label
import System.FilePath.Posix
import Data.List.Split
import Data.List (intercalate)
import Data.Maybe
import Control.Applicative

-- | Game definition for SSBM.
melee :: Game
melee = Game { title   = "SSBM"
             , widgets = [dmgStocks]
             }

dmgStocks :: Widget
dmgStocks = Widget { resolution = 32
                   , position = [(w 36, h 762), (w 336, h 762)]
                   , dimensions = (w 300, h 260)
                   , cardinalities = [10, 10, 10, 4]
                   , netSpec = [ConvS 9 64, ReLUS, PoolS, ConvS 6 64, ReLUS, PoolS]
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
                p1p' = percentLegible p1p
                p2p' = percentLegible p2p
                p1Stocks = if ingame then Label (read p1s) else Indeterminate
                p2Stocks = if ingame then Label (read p2s) else Indeterminate
                matchLength = 8 * 60 * 60
                time
                  | not ingame = replicate 3 Indeterminate
                  | ingame = let frames = matchLength - read framesPassed
                                 secsTotal = frames `div` 60
                                 (mins, secs) = secsTotal `divMod` 60
                                 (secs10, secs1) = secs `divMod` 10
                              in Label <$> [mins, secs10, secs1]

             in [[p1p' ++ [p1Stocks], p2p' ++ [p2Stocks]]]
          , cropRect = Rect 335 50 1251 1027
          , wiggle = 30
          , distort = True
          }

delabelMelee :: [Maybe Int] -> String
delabelMelee [p11, p21, p110, p210, p1100, p2100, td1, td2, td3, p1s, p2s] = filename
  where
    filename = intercalate "_" . fmap p $ [p1p, p1s, p2p, p2s, mins, secs]
    secs = liftA2 (+) ((*10) <$> td2) td3
    mins = td1
    p = maybe "X" show
    p1p = case p11 of
            Nothing   -> Nothing
            Just p11' -> Just $ p11' + fromMaybe 0 p110 * 10 + fromMaybe 0 p1100 * 100
    p2p = case p21 of
            Nothing   -> Nothing
            Just p21' -> Just $ p21' + fromMaybe 0 p210 * 10 + fromMaybe 0 p2100 * 100

delabelMelee _ = undefined
