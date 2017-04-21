{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Types
import Visor
import Lib
import IO
import Games.Melee
import Games.Melee.Conduits
import Games.Melee.Graphic
import System.Environment
import Conduit
import Buffer

type Game = Melee
type BatchSize = 16

main :: IO ()
main = getArgs >>= main'

main' ["watch", read' -> x, read' -> y, read' -> w, read' -> h] =
  do v :: Visor Game <- loadVisor
     runConduitRes$ screenShotSource x y w h
                 .| mapMC (feedImage v)
                 .| mapC delabel
                 .| bufferedFilter 5
                 .| ioC (putStrLn . pretty)
                 .| collectGame
                 .| mapMC (liftIO . gameGraph)
                 .| sinkNull

main' l = putStrLn$ "Unrecognized argument list: " ++ unwords l

