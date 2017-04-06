module Games.Melee.Conduits where

import Conduit
import Games.Melee
import Types
import Buffer

type MeleeB = Buffer Melee
type Game = Melee

groupGames :: RTConduit MeleeB [Melee]
groupGames = go Nothing
  where go last = do mbuf <- await
                     case mbuf of
                       Nothing -> return ()
                       Just (Buffer buf) | extractGame' buf == last -> go last

extractGame' :: [Melee] -> Maybe Game
extractGame' = undefined

