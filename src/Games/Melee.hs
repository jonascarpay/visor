module Games.Melee where

import Game
import Dataset
import Util
import System.Directory
import System.FilePath.Posix
import Data.List.Split
import Vision.Primitive

melee :: Game
melee = Game { title = "SSBM"
             , features = [ percentageOnes
                          , percentageTens
                          , percentagHundreds
                          ]
             , datasets = [ dolphin_sets
                          ]
             }

percentageOnes :: Feature
percentageOnes = Feature { name = "Percentage ones digit"
                         , positions = [ (214/width, 914/height)
                                       , (518/width, 914/height)
                                       ]
                         , dimensions = (100/width,120/height)
                         , resolution = (16,16)
                         }

percentageTens :: Feature
percentageTens = Feature { name = "Percentage ones digit"
                         , positions = [ (149/width, 914/height)
                                       , (442/width, 914/height)
                                       ]
                         , dimensions = (100/width,120/height)
                         , resolution = (16,16)
                         }

percentagHundreds :: Feature
percentagHundreds = Feature { name = "Percentage ones digit"
                            , positions = [ (94/width, 914/height)
                                          , (394/width, 914/height)
                                          ]
                            , dimensions = (100/width,120/height)
                            , resolution = (16,16)
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
asDigits cs = fmap read' (reverse cs) ++ repeat Nothing
  where read' '1' = Just 1
        read' '2' = Just 2
        read' '3' = Just 3
        read' '4' = Just 4
        read' '5' = Just 5
        read' '6' = Just 6
        read' '7' = Just 7
        read' '8' = Just 8
        read' '9' = Just 9
        read' '0' = Just 0
        read' _   = Nothing


dolphin_sets :: Dataset
dolphin_sets =
  Dataset { files = do subdirs <- getDirectoryContents ssbm_root
                       let subdirs' = prepend ssbm_root . filterHidden $ subdirs
                       imgs <- traverse getDirectoryContents subdirs'
                       let imgs' = fmap filterHidden imgs
                           paths = concat $ zipWith prepend subdirs' imgs'
                       return paths

          , labels = \f -> let (_:_:_:p1p:_:_:p2p:_) = splitOn "_" f
                               p1p' = take 3 (asDigits p1p)
                               p2p' = take 3 (asDigits p2p)
                            in return $ merge p1p' p2p'
          , cropRect = Rect 335 50 1251 1027
          , wiggle = 20
          , distort = True
          }
