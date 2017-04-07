{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Games.Melee.Conduits where

import Conduit
import Games.Melee
import Types
import Buffer
import Codec.Picture( PixelRGBA8( .. ), writePng )
import Graphics.Rasterific
import Graphics.Rasterific.Texture
import Data.Singletons.TypeLits
import Data.Proxy

type MeleeB s = Buffer s Melee
type MeleeGame = [Melee]

onGameEnd :: forall s. KnownNat s => (MeleeGame -> IO ()) -> RTConduit (MeleeB s) (MeleeB s)
onGameEnd handler = go False
  where

    bufsize = fromIntegral$ natVal (Proxy :: Proxy s)
    isStart buf = Menu `notElem` buf && length buf == bufsize && isStartFrame (last buf)

    isEnd buf = all (==Menu) (init buf) && length buf == bufsize && isEndFrame (last buf)

    go False = do mbuf <- await
                  case mbuf of
                    Nothing -> return ()
                    Just (Buffer (take bufsize -> buf)) ->
                      if isStart buf
                       then liftIO (putStrLn "Starting game") >> go True
                       else liftIO (putStr "F") >> go False

    go True = do mbuf <- await
                 case mbuf of
                   Nothing -> return ()
                   Just b@(Buffer buf) ->
                     if isEnd (take bufsize buf)
                        then do liftIO$ putStrLn "Game over"
                                liftIO$ handler (extractGame b)
                                go False

                        else go True

extractGame :: MeleeB s -> MeleeGame
extractGame = undefined

isStartFrame (Ingame (PlayerState 4 0) _ (PlayerState 4 0) _ ) = True
isStartFrame _ = False

isEndFrame (Win _ _) = True
isEndFrame _ = False

renderGame :: MeleeGame -> IO ()
renderGame _ =
       do let white = PixelRGBA8 255 255 255 255
              drawColor = PixelRGBA8 0 0x86 0xc1 255
              recColor = PixelRGBA8 0xFF 0x53 0x73 255
              img = renderDrawing 400 200 white $
                withTexture (uniformTexture drawColor) $ do
                  fill $ circle (V2 0 0) 30
                  stroke 4 JoinRound (CapRound, CapRound) $
                    circle (V2 400 200) 40
                  withTexture (uniformTexture recColor) .
                    fill $ rectangle (V2 100 100) 200 100

          writePng "yourimage.png" img
