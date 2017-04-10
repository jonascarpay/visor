
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Games.Melee (
  Melee (..),
  PlayerState (..),
) where

import Lib
import Vector
import Types
import Network
import Network.Label
import Layers
import Data.Singletons.Prelude.List
import Data.List.Split
import Control.Monad
import System.FilePath.Posix
import Buffer

-- | Game definition for SSBM.
data Melee = Menu
           | Ingame !PlayerState !Int !PlayerState !Int
           | Win !PlayerState !Int
         deriving (Eq, Show)

-- | The state of a player in a game
data PlayerState = PlayerState
  { stocks :: !Int  -- ^ Stocks
  , percent :: !Int -- ^ Percentage
  } deriving (Eq, Show)

instance Pretty PlayerState where
  pretty (PlayerState s p) = stockstring s ++ " " ++ show p ++ "%"
    where
      stockstring n = replicate n 'O' ++ replicate (4-n) ' '

instance Transitions PlayerState where
  PlayerState s p ->? PlayerState s' p'
    | s' == s && p' >= p && p' - p < 80 = True
    | p  >  0 && p' == 0                = s' == s - 1
    | otherwise                         = False

instance Transitions Melee where
  a ->? b | a == b = True

  Menu ->? Menu = True
  Menu ->? Ingame (PlayerState 4 0) _ (PlayerState 4 0) _ = True

  Win _ _   ->? Menu      = True

  Ingame p1 q1 p2 q2 ->? Win p q
    | stocks p1 == 1 && p == p2 && q == q2 = True
    | stocks p2 == 1 && p == p1 && q == q1 = True
  Ingame p1 q1 p2 q2 ->? Ingame p1' q1' p2' q2' = and [ q1 ==  q1', q2 ==  q2'
                                                      , p1 ->? p1', p2 ->? p2' ]
  _ ->? _ = False

instance GameState Melee where
  type Title        Melee = "Melee"
  type ScreenWidth  Melee = 584
  type ScreenHeight Melee = 480
  type Widgets      Melee = '[Melee]
  label st = LabelVec$ toLabel st :- Nil
  delabel (LabelVec (WLabel l :- Nil)) =
    case parseLabel l (fromLabel :: LabelParser Melee) of
        Left err -> error err
        Right s  -> s

  rootDir = Path "/Users/joni/tmp/"
  parse = fromFilename

instance Pretty Melee where
  pretty Menu = "Menu"
  pretty (Win p q) = "Player " ++ show q ++ " wins with " ++ pretty p
  pretty (Ingame p1 q1 p2 q2)
    =     "P" ++ show q1 ++ ": " ++ pretty p1 ++
      "\t\tP" ++ show q2 ++ ": " ++ pretty p2

instance Widget Melee where
  type Width     Melee = 140
  type Height    Melee = 120
  type Parent    Melee = Melee
  type DataShape Melee = '[ 10, 10, 10, 5 ]
  type Positions Melee = '[ '(16,  356)
                          , '(156, 356)
                          , '(294, 356)
                          , '(432, 356) ]

  type SampleWidth  Melee = 42
  type SampleHeight Melee = 42
  type NetConfig    Melee = '[ Convolution 12 3 5 5 38 38
                             , Pool
                             , ReLU
                             , Convolution 32 12 8 8 12 12
                             , ReLU
                             , Flatten
                             , FC 4608 1024
                             , ReLU
                             , FC 1024 (Sum (DataShape Melee))
                             , MultiSoftMax (DataShape Melee)
                             ]

  params = Params (LearningParameters 1e-4 0.9 1e-7)

  toLabel Menu = WLabel$ fill 0
  toLabel (Win p q) = WLabel$ get 1 <-> get 2 <-> get 3 <-> get 4
    where get n
            | n == q   = playerLabel p
            | otherwise = fill 0
  toLabel (Ingame p1 q1 p2 q2) = WLabel$ get 1 <-> get 2 <-> get 3 <-> get 4
    where get n
            | n == q1   = playerLabel p1
            | n == q2   = playerLabel p2
            | otherwise = fill 0

  fromLabel = do players <- replicateM 4 playerParser
                 case count ingame players of
                   2 -> let [(p1, q1), (p2, q2)] = filter (ingame.fst) $ zip players [1..]
                         in return$! Ingame p1 q1 p2 q2
                   1 -> return$! let (p,q) = head (filter (ingame.fst) $ zip players [1..]) in Win p q
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

fromFilename :: Path a -> LabelVec Melee
fromFilename (Path (wordsBy (=='_') . takeWhile (/='.') . takeBaseName
  -> [ "shot", _, "psd", psd, "st", st
     , "p1", "g", g1, "c", _, "s", read' -> s1, "p", read' -> p1
     , "p2", "g", g2, "c", _, "s", read' -> s2, "p", read' -> p2
     , "p3", "g", g3, "c", _, "s", read' -> s3, "p", read' -> p3
     , "p4", "g", g4, "c", _, "s", read' -> s4, "p", read' -> p4
     ] ))

  | psd == "1" || st == "0" = LabelVec$ WLabel (fill 0) :- Nil
  | otherwise = LabelVec$
                WLabel (get g1 s1 p1 <->
                        get g2 s2 p2 <->
                        get g3 s3 p3 <->
                        get g4 s4 p4) :- Nil

   where get "0" _ _ = fill 0
         get "1" s p = playerLabel $ mkPlayerState s p
         get  _  _ _ = error "Error while parsing filename"

fromFilename (Path p) = error$ "Invalid filename: " ++ p

