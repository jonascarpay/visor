{-# LANGUAGE ViewPatterns #-}

module Games.Melee where

import Game
import ConvNet
import Util
import Label
import Conduit
import System.FilePath.Posix
import Data.List.Split

-- | Game definition for SSBM.
melee :: Game
melee = Game { title   = "SSBM"
             , widgets = [dmgStocks]
             }

dmgStocks :: Widget
dmgStocks = Widget { resolution = 32
                   , position = [(w 36, h 762), (w 336, h 762)]
                   , dimensions = (w 300, h 260)
                   , cardinalities = [10, 10, 10, 5]
                   , netSpec = [ConvS 13 64, ReLUS, PoolS, ConvS 5 64, ReLUS, PoolS]
                   }

-- Time is not yet included. It adds little important information, and is pretty inefficient
-- until support for non-square inputs
time :: Widget
time = Widget { resolution = 40
              , position = [(w 482, h 42)]
              , dimensions = (w 262, h 262)
              , cardinalities = [10,10,10]
              , netSpec = [ConvS 13 16, ReLUS, PoolS, ConvS 5 16, ReLUS, PoolS]
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
                p1p' = if p1s /= "0" then percentLegible p1p else replicate 3 Indeterminate
                p2p' = if p2s /= "0" then percentLegible p2p else replicate 3 Indeterminate
                p1Stocks = if ingame then Label (read p1s) else Label 0
                p2Stocks = if ingame then Label (read p2s) else Label 0
                matchLength = 8 * 60 * 60
                time
                  | not ingame = replicate 3 Indeterminate
                  | otherwise  = let frames = matchLength - read framesPassed
                                     secsTotal = frames `div` 60
                                     (mins, secs) = secsTotal `divMod` 60
                                     (secs10, secs1) = secs `divMod` 10
                                  in Label <$> [mins, secs10, secs1]

             in [ [p1p' ++ [p1Stocks], p2p' ++ [p2Stocks]]
                , [time]
                ]
          , cropRect = Rect 335 50 1251 1027
          , wiggle = 30
          , distort = True
          }

parseState :: [[WidgetLabel]] -> Maybe MeleeState
parseState [[p1w, p2w]] = case (parseWidget p1w, parseWidget p2w) of
                            (Just (p1p, p1s), Just (p2p, p2s)) -> Just $ Ingame p1p p1s p2p p2s
                            (Nothing, Nothing)                 -> Just Menu
                            _                                  -> Nothing
  where
    parseLabel Indeterminate = 0
    parseLabel (Label n) = n

    parseWidget [_, _, _, Indeterminate] = Nothing
    parseWidget [Indeterminate, _, _, _] = Nothing
    parseWidget [parseLabel -> p1, parseLabel -> p10, parseLabel -> p100, parseLabel -> stocks] = Just (p1 + 10*p10 + 100*p100, stocks)
    parseWidget _ = undefined
parseState _ = undefined

data MeleeState = Menu
                | Ingame { p1p :: Int
                         , p1s :: Int
                         , p2p :: Int
                         , p2s :: Int
                         } deriving Eq

showMelee Menu = "Not in game"
showMelee (Ingame p1p p1s p2p p2s) = show p1p ++ ' ':show p1s ++ '\t':show p2p ++ ' ':show p2s

newGame :: MeleeState
newGame = Ingame 0 4 0 4

validate :: MeleeState -> MeleeState -> Bool
validate Menu Menu = True
validate (Ingame p1p' p1s' p2p' p2s') (Ingame p1p p1s p2p p2s) = and [ p1s' <= p1s , p2s' <= p2s
                                                                     , p1p' >= p1p || p1p' == 0
                                                                     , p2p' >= p2p || p2p' == 0
                                                                     ]
validate _ _ = False

updateMelee :: Maybe MeleeState -> (MeleeState, [MeleeState]) -> (MeleeState, [MeleeState])
updateMelee Nothing m = m
updateMelee _ (_, []) = error "uninitialized game state"
updateMelee (Just m') (mBuf, m:ms)
  | m' == mBuf && m' `validate` m = (m', m':m:ms)
  | otherwise = (m', m:ms)

meleeC :: IOSink [[WidgetLabel]]
meleeC = go (newGame, [newGame])
  where
    go st = do ms <- await
               case ms of
                 Nothing -> return ()
                 Just s ->
                   let st' = updateMelee (parseState s) st
                       (_, o:_) = st'
                    in do liftIO$ putStrLn (showMelee o)
                          go st'
