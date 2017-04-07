module Games.Melee.Conduits where

import Conduit
import Games.Melee
import Types
import Buffer
import Codec.Picture( PixelRGBA8( .. ), writePng )
import Graphics.Rasterific
import Graphics.Rasterific.Texture
import Data.Singletons.TypeLits

type MeleeB s = Buffer s Melee
type MeleeGame = [Melee]

onGameEnd :: KnownNat s => (MeleeGame -> IO ()) -> RTConduit (MeleeB s) (MeleeB s)
onGameEnd handler = go False
  where
    go ingame = do mbuf <- await
                   case (ingame, mbuf) of
                      (_, Nothing) -> return ()
                      (False, Just buf) ->
                        if maybe False isStartFrame (bufHead buf)
                         then liftIO (putStrLn "Starting game") >> go True
                         else liftIO (putStr "F") >> go False
                      (True, Just buf) ->
                        if maybe True isEndFrame (bufHead buf)
                         then do liftIO$ putStrLn "Game over"
                                 liftIO$ handler (extractGame buf)
                                 go False
                         else liftIO (putStr "T") >> go False

extractGame :: MeleeB s -> MeleeGame
extractGame = undefined

isStartFrame, isEndFrame :: Melee -> Bool
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
