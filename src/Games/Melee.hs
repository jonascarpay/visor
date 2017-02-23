{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Games.Melee (
  Melee (..),
) where

import Lib
import Vector
import Types
import Network.Label
import Layers
import Data.Singletons.Prelude.List
import Data.List.Split
import Control.Monad

-- | Game definition for SSBM.
data Melee = Menu
           | Ingame !PlayerState !Int !PlayerState !Int
         deriving (Eq, Show)

-- | The state of a player in a game
data PlayerState = PlayerState
  !Int -- ^ Stocks
  !Int -- ^ Percentage
  deriving (Eq, Show)

instance Transitions PlayerState where
  PlayerState s p ->? PlayerState s' p'
    | s' == s && p' >= p = True
    | p  >  0 && p' == 0 = s' == s - 1
    | otherwise          = False

instance Transitions Melee where
  Menu ->? Menu = True
  Menu ->? Ingame (PlayerState 4 0) _ (PlayerState 4 0) _ = True

  Ingame (PlayerState 0 0) _ _ _ ->? Menu = True
  Ingame _ _ (PlayerState 0 0) _ ->? Menu = True
  Ingame p1 s1 p2 s2 ->? Ingame p1' s1' p2' s2' = and [ s1 ==  s1', s2 ==  s2'
                                                      , p1 ->? p1', p2 ->? p2' ]

  _ ->? _ = False

instance GameState Melee where
  type Title        Melee = "Melee"
  type ScreenWidth  Melee = 584
  type ScreenHeight Melee = 480
  type Widgets      Melee = '[Melee]
  label st = toLabel st :- Nil
  dataset = dolphinShots
  delabel (WLabel l :- Nil) = case parseLabel l (fromLabel :: LabelParser Melee) of
                                Left err -> error err
                                Right s  -> s

instance Widget Melee where
  type Width     Melee = 140
  type Height    Melee = 96
  type Parent    Melee = Melee
  type DataShape Melee = '[ 10, 10, 10, 5 ]
  type Positions Melee = '[ '(16,  356)
                          , '(156, 356)
                          , '(294, 356)
                          , '(432, 356) ]

  type SampleWidth  Melee = 32
  type SampleHeight Melee = 32
  type NetConfig    Melee = '[ Convolution 16 3 9 9 24 24
                             , Pool
                             , Pool
                             , ReLU
                             , Flatten
                             , FC 576 100
                             , ReLU
                             , FC 100 (Sum (DataShape Melee))
                             , MultiSoftMax (DataShape Melee)
                             ]


  toLabel Menu = WLabel$ fill 0
  toLabel (Ingame p1 s1 p2 s2) = WLabel$ get 1 <-> get 2 <-> get 3 <-> get 4
    where get n
            | n == s1   = playerLabel p1
            | n == s2   = playerLabel p2
            | otherwise = fill 0

  fromLabel = do players <- replicateM 4 playerParser
                 case count ingame players of
                   2 -> let [(p1, s1), (p2, s2)] = filter (ingame.fst) $ zip players [1..]
                         in return$! Ingame p1 s1 p2 s2
                   _ -> return Menu

ingame :: PlayerState -> Bool
ingame (PlayerState 0 _) = False
ingame _ = True

playerLabel :: PlayerState -> LabelComposite 1 '[10, 10, 10, 5]
playerLabel (PlayerState 0 _) = fill 0
playerLabel (PlayerState stocks percent) =
       singleton d100
   <|> singleton d10
   <|> singleton d1
   <|> singleton stocks
  where (d100, d10, d1) = splitDigits percent

playerParser :: LabelParser PlayerState
playerParser = do d100   <- pop
                  d10    <- pop
                  d1     <- pop
                  stocks <- pop
                  let dmg = 100 * d100 + 10 * d10 + 1 * d1
                  return $! mkPlayerState stocks dmg

mkPlayerState :: Int -> Int -> PlayerState
mkPlayerState 0 _ = PlayerState 0 0
mkPlayerState s p
  | s < 0 || s > 4   = error$ "PlayerState with " ++ show s ++ " stocks"
  | p < 0 || p > 999 = error$ "PlayerState with " ++ show p ++ " percent"
  | otherwise      = PlayerState s p

dolphinShots :: Dataset Melee
dolphinShots =
  Dataset { rootDir = "/Users/jmc/tmp/"
          , parseFilename = fromFilename
          }

fromFilename :: FilePath -> LabelVec Melee
fromFilename (wordsBy (=='_') -> ["shot", _, "psd", psd, "st", _,
                                  "p1", "g", g1, "c", _, "s", read -> s1 :: Int, "p", read -> p1 :: Int,
                                  "p2", "g", g2, "c", _, "s", read -> s2 :: Int, "p", read -> p2 :: Int,
                                  "p3", "g", g3, "c", _, "s", read -> s3 :: Int, "p", read -> p3 :: Int,
                                  "p4", "g", g4, "c", _, "s", read -> s4 :: Int, "p", read -> p4 :: Int] )
  | psd == "1" = WLabel (fill 0) :- Nil
  | otherwise  = WLabel ((get g1 s1 p1) <->
                         (get g2 s2 p2) <->
                         (get g3 s3 p3) <->
                         (get g4 s4 p4)) :- Nil

   where get "0" _ _ = fill 0
         get "1" s p = playerLabel $ mkPlayerState s p

fromFilename _ = error "Invalid filename"

