{-# LANGUAGE ViewPatterns #-}

module Games.MeleeWatch where

import Game
import Games.Melee
import Games.MeleeDiagram
import Util
import Label
import Conduit

parseState :: [[WidgetLabel]] -> Maybe GameState
parseState [[p1w, p2w]] = case (parseWidget p1w, parseWidget p2w) of
                            (Nothing, Nothing)                 -> Just Menu
                            (Just (0,0), Just (0,0))           -> Just Menu
                            (Just (p1p, p1s), Just (p2p, p2s)) -> Just $ Ingame p1p p1s p2p p2s
                            _                                  -> Nothing
  where
    parseLabel Indeterminate = 0
    parseLabel (Label n) = n

    parseWidget [Indeterminate, _, _, Indeterminate] = Just (0,0)
    parseWidget [Indeterminate, _, _, parseLabel -> 0] = Just (0,0)
    parseWidget [_, _, _, Indeterminate] = Nothing
    parseWidget [Indeterminate, _, _, _] = Nothing
    parseWidget [parseLabel -> p1, parseLabel -> p10, parseLabel -> p100, parseLabel -> stocks] = Just (p1 + 10*p10 + 100*p100, stocks)
    parseWidget _ = undefined
parseState _ = undefined

gameOver :: GameState -> Bool
gameOver Menu = False
gameOver (Ingame _ 0 _ 0) = False
gameOver (Ingame _ _ _ 0) = True
gameOver (Ingame _ 0 _ _) = True
gameOver _ = False

validate :: GameState -> GameState -> Bool
validate Menu Menu = True
validate Menu (Ingame _ s1 _ s2) = s1 < 2 || s2 < 2
validate (Ingame 0 4 0 4) Menu = True
validate (Ingame p1p' p1s' p2p' p2s') (Ingame p1p p1s p2p p2s) = and [ p1s' <= p1s , p2s' <= p2s
                                                                     , p1p' >= p1p && p1s' == p1s || p1p' == 0 && p1s' == p1s -1
                                                                     , p2p' >= p2p && p2s' == p2s || p2p' == 0 && p2s' == p2s -1
                                                                     ]
validate _ _ = False

updateMelee :: Maybe GameState -> (GameState, [GameState]) -> (GameState, [GameState])
updateMelee Nothing m = m
updateMelee _ (_, []) = error "uninitialized game state"
updateMelee (Just m') (mBuf, m:ms)
  | mBuf == m' && m' `validate` m = (m', m':m:ms)
  | otherwise = (m', m:ms)

meleeC :: IOSink [[WidgetLabel]]
meleeC = go newgame []
  where
    newgame = (Menu, [Menu])
    go st gs = do ms <- await
                  case ms of
                    Nothing -> return ()
                    Just s ->
                      let st' = updateMelee (parseState s) st
                          (_, currentFrame:_) = st'
                          gameNr = length gs + 1
                       in do liftIO$ putStrLn ("Game " ++ show gameNr ++ '\t':showMelee currentFrame ++ ' ':show currentFrame)
                             if gameOver currentFrame
                                then liftIO (gameDiagram (snd st'))
                                else go st' gs
