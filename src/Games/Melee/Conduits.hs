module Games.Melee.Conduits where

import Conduit
import Games.Melee
import Games.Melee.Graphic
import Types

collectGame :: RTConduit Melee MeleeGame
collectGame = go mempty
  where
    go log = do mst <- await
                case (log, mst) of
                   (_, Nothing) ->
                     return ()
                   ([], Just st) ->
                     go [st | isStartFrame st]
                   (log, Just st) ->
                     if isEndFrame st
                        then do yield (st:log)
                                go []
                        else liftIO (putStr "\129302 ") >> go (st:log)

isStartFrame, isEndFrame :: Melee -> Bool
isStartFrame (Ingame (PlayerState 4 0) _ (PlayerState 4 0) _ ) = True
isStartFrame _ = False

isEndFrame (Win _ _) = True
isEndFrame _ = False
