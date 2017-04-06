module Games.Melee.Conduits where

import Conduit
import Games.Melee
import Types
import Data.List

type MeleeBuf = [Melee]
type Game = Melee

groupGames :: RTConduit MeleeBuf [Melee]
groupGames = go Nothing
  where go last = do mbuf <- await
                     case mbuf of
                       Nothing -> return ()
                       Just buf | extractGame' buf == last -> go last

extractGame' :: [Melee] -> Maybe Game
extractGame' = undefined

newtype Parser a = Parser (MeleeBuf -> (a, MeleeBuf))
instance Functor Parser where
  fmap f (Parser x) = Parser$ \b -> undefined

extractGame :: [Melee] -> (Maybe Game, MeleeBuf)
extractGame = undefined
