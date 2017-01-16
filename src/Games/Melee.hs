module Games.Melee where

import Game
import ConvNet

-- | Game definition for SSBM.
data Melee = Menu
           | Ingame2P PlayerState PlayerState
           | Ingame4P PlayerState PlayerState PlayerState PlayerState
         deriving (Eq, Show)

data PlayerState = PlayerState { stocks :: Int
                               , percent :: Int
                               } deriving (Eq, Show)

instance GameState Melee where

dmgStocks :: Widget Melee
dmgStocks = Widget { resolution = 32
                   , position = [(w 36, h 762), (w 336, h 762)]
                   , dimensions = (w 300, h 260)
                   , cardinalities = [10, 10, 10, 5]
                   , netSpec = [ConvS 13 64, ReLUS, PoolS, ConvS 5 64, ReLUS, PoolS]
                   }

-- Time is not yet included. It adds little important information, and is pretty inefficient
-- until support for non-square inputs
time :: Widget Melee
time = Widget { resolution = 40
              , position = [(w 482, h 42)]
              , dimensions = (w 262, h 262)
              , cardinalities = [10,10,10]
              , netSpec = [ConvS 9 16, PoolS, ReLUS, ConvS 5 16, PoolS, ReLUS]
              }

w, h :: Double -> Double
w x = x / screenWidth
  where screenWidth = 1252
h y = y / screenHeight
  where screenHeight = 1028

dolphinShots :: Dataset Melee
dolphinShots =
  Dataset { rootDir = "/Users/jmc/tmp/"
          , cropRect = Nothing
          , wiggle = 30
          , distort = True
          }
