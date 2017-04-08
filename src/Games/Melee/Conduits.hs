module Games.Melee.Conduits where

import Conduit
import Games.Melee
import Types
import Codec.Picture( PixelRGBA8( .. ), writePng )
import Graphics.Rasterific
import Graphics.Rasterific.Texture

type MeleeGame = [Melee]

collectGame :: RTConduit Melee MeleeGame
collectGame = go []
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

gameGraph :: RTSink (MeleeGame)
gameGraph = awaitForever$ \_ ->
  do let white     = PixelRGBA8 255 255 255 255
         drawColor = PixelRGBA8 0 0x86 0xc1 255
         recColor  = PixelRGBA8 0xFF 0x53 0x73 255
         img       = renderDrawing 400 200 white $
           withTexture (uniformTexture drawColor) $ do
             fill $ circle (V2 0 0) 30
             stroke 4 JoinRound (CapRound, CapRound) $
               circle (V2 400 200) 40
             withTexture (uniformTexture recColor) .
               fill $ rectangle (V2 100 100) 200 100

     liftIO$ writePng "yourimage.png" img
