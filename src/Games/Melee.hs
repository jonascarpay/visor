{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Games.Melee where

import Util
import ConvNet
import Data.List.Split
import Types

-- | The state of a player in a game
data PlayerState = PlayerState { stocks :: Int
                               , percent :: Int
                               } deriving (Eq, Show)

instance Transitions PlayerState where
  PlayerState s p ->? PlayerState s' p'
    | s' == s && p' >= p = True
    | p  >  0 && p' == 0 = s' == s - 1
    | otherwise          = False

instance WidgetData PlayerState

-- | Game definition for SSBM.
data Melee = Menu
           | Ingame2P PlayerState Int PlayerState Int
           | Ingame4P PlayerState PlayerState PlayerState PlayerState
         deriving (Eq, Show)

instance Transitions GameState where
  Menu ->? Menu                                                                             = True
  Menu ->? Ingame2P (PlayerState 0 0) _ (PlayerState 0 0) _                                 = True
  Menu ->? Ingame4P (PlayerState 0 0) (PlayerState 0 0) (PlayerState 0 0) (PlayerState 0 0) = True

  Ingame2P (PlayerState 0 0) _ ->? Menu = True
  Ingame2P _ (PlayerState 0 0) ->? Menu = True
  Ingame2P p1 p2 ->? Ingame2P p1' p2'   = p1 ->? p1' && p2 ->? p2'

  Ingame4P p1 p2 p3 p4 ->? Menu                     = count (PlayerState 0 0) [p1, p2, p3, p4] > 1
  Ingame4P p1 p2 p3 p4 ->? Ingame4P p1' p2' p3' p4' = p1 == p1' && p2 == p2' && p3 == p3' && p4 == p4'

  _ ->? _ = False

instance GameState Melee where

dmgStocks :: Widget Melee
dmgStocks = Widget { resolution = 32
                   , position = [(w 36, h 762), (w 336, h 762)]
                   , dimensions = (w 300, h 260)
                   , cardinalities = [10, 10, 10, 5]
                   , netSpec = [ConvS 13 64, ReLUS, PoolS, ConvS 5 64, ReLUS, PoolS]
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

