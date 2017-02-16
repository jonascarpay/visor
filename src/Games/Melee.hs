{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Games.Melee where

import Util
import Network.Label
import Data.List.Split
import Data.Singletons.TypeLits
import Data.Singletons.Prelude.Num
import Data.Singletons.Prelude
import Types

-- | Game definition for SSBM.
data Melee = Menu
           | Ingame PlayerState Int PlayerState Int
         deriving (Eq, Show)

-- | The state of a player in a game
data PlayerState = PlayerState { stocks  :: !Int
                               , percent :: !Int
                               } deriving (Eq, Show)

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
  Ingame p1 s1 p2 s2 ->? Ingame p1' s1' p2' s2'   = and [ s1 ==  s1', s2 ==  s2'
                                                        , p1 ->? p1', p2 ->? p2' ]

  _ ->? _ = False

instance GameState Melee where
  type Title        Melee = "Melee"
  type ScreenWidth  Melee = 584
  type ScreenHeight Melee = 480

instance Widget Melee where
  type Width     Melee = 140
  type Height    Melee = 96
  type DataShape Melee = '[ 10, 10, 10, 5 ]
  type Positions Melee = '[ '(16,  356)
                          , '(156, 356)
                          , '(294, 356)
                          , '(432, 356) ]

  toLabel Menu = noParse
  toLabel (Ingame p1 s1 p2 s2) = undefined

type Label' c = LabelSingle (c :+ 1)
digit x = singleton (x+1) :: Label' 10
stock x = singleton (x+1) :: Label' 5
noParse = maxed

playerLabel :: PlayerState -> LabelComposite 1 '[10, 10, 10, 5]
playerLabel (PlayerState 0 _) = fill 0
playerLabel (PlayerState stocks percent) =
       singleton d100
   <|> singleton d10
   <|> singleton d1
   <|> singleton stocks
  where (d100, d10, d1) = splitDigits percent

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

