module Games.Melee where

import Game
import Util
import System.FilePath.Posix
import Data.List.Split
import Vision.Primitive

-- | Game definition for SSBM.
melee :: Game
melee = Game { title = "SSBM"
             , features = [percentageOnes, percentageTens, percentage100s, timeDigits, stocks, stage]
             }

percentageOnes :: Feature
percentageOnes = Feature { name        = "onesDigit"
                         , positions   = [ (w 214, h 914)
                                         , (w 516, h 914)
                                         ]
                         , dimensions  = (w 100,h 120)
                         , resolution  = (16,16)
                         , cardinality = 10
                         , netConfig   = defaultNetConfig
                         }

percentageTens :: Feature
percentageTens = Feature { name        = "tensDigit"
                         , positions   = [ (w 155, h 914)
                                         , (w 461, h 914)
                                         ]
                         , dimensions  = (w 120, h 120)
                         , resolution  = (16,16)
                         , cardinality = 10
                         , netConfig   = defaultNetConfig
                         }

percentage100s :: Feature
percentage100s = Feature { name        = "hundredsDigit"
                         , positions   = [ (w 94, h 914)
                                         , (w 404, h 914)
                                         ]
                         , dimensions  = (w 120, h 120)
                         , resolution  = (16,16)
                         , cardinality = 10
                         , netConfig   = defaultNetConfig
                         }

timeDigits :: Feature
timeDigits = Feature { name        = "timeDigit"
                     , positions   = [ (w 563, h 142)
                                     , (w 641, h 142)
                                     , (w 698, h 142)
                                     ]
                     , dimensions  = (w 74, h 74)
                     , resolution  = (16,16)
                     , cardinality = 10
                     , netConfig   = defaultNetConfig
                     }

stocks :: Feature
stocks = Feature { name        = "stockCount"
                 , positions   = [ (w 150, h 802)
                                 , (w 453, h 802)
                                 ]
                 , dimensions  = (w 228, h 74)
                 , resolution  = (32,8)
                 , cardinality = 4
                 , netConfig   = defaultNetConfig
                 }

stage :: Feature
stage = Feature { name        = "stockCount"
                , positions   = [ (w 666, h 555) ]
                , dimensions  = (w 1246, h 584)
                , resolution  = (32,16)
                , cardinality = 4
                , netConfig   = defaultNetConfig
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
asDigits :: String -> [Maybe Int]
asDigits cs = fmap readDigit (reverse cs) ++ repeat Nothing

dolphin_sets :: Dataset
dolphin_sets =
  Dataset { rootDir = "/Users/jmc/tmp/"
          , labels = \f ->
            let ["shot",_,stage,framesPassed,p1p,p1s,p2p,p2s] = splitOn "_" (takeBaseName f)
                stage' = Just $ read stage
                ingame = stage /= "0"
                percentLegible p = take 3 $ if ingame && p /= "X" then asDigits p else repeat Nothing
                p1p' = percentLegible p1p
                p2p' = percentLegible p2p
                p1Stocks = if ingame then Just (read p1s) else Nothing
                p2Stocks = if ingame then Just (read p2s) else Nothing
                matchLength = 8 * 60 * 60
                time = if ingame
                          then let frames = matchLength - read framesPassed
                                   secsTotal = frames `div` 60
                                   (mins, secs) = secsTotal `divMod` 60
                                   (secs10, secs1) = secs `divMod` 10
                                in Just <$> [mins, secs10, secs1]
                          else replicate 3 Nothing
                       in merge p1p' p2p' ++ time ++ [p1Stocks, p2Stocks, stage']
          , cropRect = Rect 335 50 1251 1027
          , wiggle = 20
          , distort = True
          }
