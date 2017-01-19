{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Games.Melee where

import Game
import Util
import ConvNet
import Data.List.Split

-- | The state of a player in a game
data PlayerState = PlayerState { stocks :: Int
                               , percent :: Int
                               } deriving (Eq, Show)

instance Transitions PlayerState where
  PlayerState s1 p1 ->? PlayerState s2 p2
    | s1 == s2 && p1 <= p2 = True
    | p1 >  0  && p2 == 0  = s2 == s1 - 1
    | otherwise            = False

-- | Game definition for SSBM.
data Melee = Menu
           | Ingame2P PlayerState PlayerState
           | Ingame4P PlayerState PlayerState PlayerState PlayerState
         deriving (Eq, Show)

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
          , parseFilename = fromFilename
          , wiggle = 30
          , distort = True
          }

fromFilename (wordsBy (=='_') -> ["shot", _, "psd", psd, "st", _,
                                  "p1", "g", g1, "c", _, "s", read -> s1 :: Int, "p", read -> p1 :: Int,
                                  "p2", "g", g2, "c", _, "s", read -> s2 :: Int, "p", read -> p2 :: Int,
                                  "p3", "g", g3, "c", _, "s", read -> s3 :: Int, "p", read -> p3 :: Int,
                                  "p4", "g", g4, "c", _, "s", read -> s4 :: Int, "p", read -> p4 :: Int] )
  | count "1" [g1,g2,g3,g4] == 2 = undefined
  | count "1" [g1,g2,g3,g4] == 4 = undefined
  | otherwise = undefined

fromFilename _ = error "Invalid filename"

