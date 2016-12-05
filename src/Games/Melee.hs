module Games.Melee where

import Game
import Util
import System.FilePath.Posix
import Data.List.Split
import Vision.Primitive

melee :: Game
melee = Game { title = "SSBM"
             , features = [ percentageOnes
                          ]
             }

percentageOnes :: Feature
percentageOnes = Feature { name = "onesDigit"
                         , positions = [ (214/width, 914/height)
                                       ]
                         , dimensions = (100/width,120/height)
                         , resolution = (16,16)
                         , cardinality = 10
                         }

width, height :: Double
width = 1252
height = 1028

ssbm_root :: FilePath
ssbm_root = "/Users/jmc/tmp/"

filterHidden :: [FilePath] -> [FilePath]
filterHidden = filter ((/= '.') . head)

prepend :: FilePath -> [FilePath] -> [FilePath]
prepend fBase = fmap (fBase</>)

-- Gives the digits in cs in order of increasing significance
-- "123" -> [Just 3, Just 2, Just 1, Nothing, Nothing..
asDigits :: String -> [Maybe Int]
asDigits cs = fmap readDigit (reverse cs) ++ repeat Nothing

dolphin_sets :: Dataset
dolphin_sets =
  Dataset { rootDir = "/Users/jmc/tmp/"
          , labels = \f -> let (_:_:_:p1p:_:_:p2p:_) = splitOn "_" (takeBaseName f)
                               p1p' = take 3 (asDigits p1p)
                               p2p' = take 3 (asDigits p2p)
                            in merge p1p' p2p'
          , cropRect = Rect 335 50 1251 1027
          , wiggle = 20
          , distort = True
          }
