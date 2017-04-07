{-# LANGUAGE ViewPatterns #-}
module Games.Melee.Conduits where

import Conduit
import Games.Melee
import Types
import Buffer

type MeleeB = Buffer Melee
type MeleeGame = [Melee]

onGameEnd :: Int -> (MeleeGame -> IO ()) -> RTConduit MeleeB MeleeB
onGameEnd bufsize handler = go False
  where
    go False = do mbuf <- await
                  case mbuf of
                    Nothing -> return ()
                    Just (Buffer (take bufsize -> buf)) ->
                      if Menu `notElem` buf    &&
                         length buf == bufsize &&
                         isStartFrame (last buf)
                       then liftIO (putStrLn "Starting game") >> go True
                       else liftIO (putStr "F") >> go False

    go True = do mbuf <- await
                 case mbuf of
                   Nothing -> return ()
                   Just (Buffer (take bufsize -> buf)) ->
                     if all (==Menu) (init buf) &&
                        length buf == bufsize
                      then liftIO (putStrLn "Game over") >> go False
                      else go True


isStartFrame (Ingame (PlayerState 4 0) _ (PlayerState 4 0) _ ) = True
isStartFrame _ = False

isEndFrame (Ingame (PlayerState s1 _) _ (PlayerState s2 _) _) = s1 == 1 || s2 == 1
isEndFrame _ = False
