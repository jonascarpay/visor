module Games.MeleeDiagram where

import Games.Melee

gameDiagram :: [GameState] -> IO ()
gameDiagram states = do print [killPcts1 states, killPcts2 states]
                        --putStrLn.unlines $ show <$> states

killPcts1 (Ingame _ p1s' _ _:s2@(Ingame p1p p1s _ _):tail)
  | p1s' == p1s-1 = p1p:killPcts1 tail
  | otherwise     = killPcts1 $ s2:tail
killPcts1 (_:tail) = killPcts1 tail
killPcts1 _ = []

killPcts2 (Ingame _ _ _ p2s' :s2@(Ingame _ _ p2p p2s):tail)
  | p2s' == p2s-1 = p2p:killPcts2 tail
  | otherwise     = killPcts2 $ s2:tail
killPcts2 (_:tail) = killPcts2 tail
killPcts2 _ = []
