module Games.Melee.Graphic where

import Games.Melee
import Types
import Graphics.Rasterific
import Graphics.Rasterific.Texture
import Codec.Picture( PixelRGBA8( .. ), writePng )
import Data.Foldable (traverse_)
import Conduit

data BarFold = BF
  { n :: Float
  , p1death :: Maybe Int
  , p1s :: Int
  , p1p :: Int
  , p1dp :: Int
  , p1energy :: Float
  , p2death :: Maybe Int
  , p2s :: Int
  , p2p :: Int
  , p2dp :: Int
  , p2energy :: Float
  }

type MeleeGame = [Melee]

gameGraph :: MeleeGame -> IO ()
gameGraph (w@(Win (PlayerState ws wp) wq):game') =
    do writePng "output.png" img
  where
    game = reverse game'
    pcts = map (\(Ingame (PlayerState _ p1) _ (PlayerState _ p2) _)
                    -> (fromIntegral p1, fromIntegral p2)
               ) game

    (!p1Max, !p2Max) = let (p1s, p2s) = unzip pcts in (maximum p1s, maximum p2s)
    (p1q, p2q) = let (Ingame _ q1 _ q2) = head game in (q1,q2)

    imageWidth  = 584
    imageHeight = 480

    frames     = length game
    graphWidth = 450
    xOffset    = (fromIntegral imageWidth - graphWidth) / 2
    yOffset    = 2
    baseY      = fromIntegral imageHeight / 2
    barWidth   = graphWidth / fromIntegral frames
    barHeight  = 100

    eHeight = 3

    p1Color = PixelRGBA8 100 30 20 255
    p1e e   = PixelRGBA8 200 p  p  255
      where p = round$ e * 100 + 50

    p2Color = PixelRGBA8 20 30 100 255
    p2e e   = PixelRGBA8 p  p  200 255
      where p = round$ e * 100 + 50

    bgColor     = PixelRGBA8 10 10 10 255
    markerColor = PixelRGBA8 255 255 255 255

    friction = 0.9
    bars     = foldbars Nothing game

    foldbars :: Maybe BarFold -> [Melee] -> [BarFold]

    foldbars Nothing (_:t) =
      b' : foldbars (Just b') t
        where b' = BF 0 Nothing 4 0 0 0 Nothing 4 0 0 0

    foldbars (Just (BF n _ s1 p1 _ e1 _ s2 p2 _ e2)) [] =
      [if wq == p1q
          then BF (n+1) Nothing   s1 p1 0 0 (Just p2) s2 p2 0 0
          else BF (n+1) (Just p1) s1 p1 0 0 Nothing   s2 p2 0 0
      ]

    foldbars
      (Just (BF n _ s1 p1 _ e1 _ s2 p2 _ e2))
      (Ingame (PlayerState s1' p1') _ (PlayerState s2' p2') _:t) =
        b' : foldbars (Just b') t
          where b' = BF (n+1) d1' s1' p1' dp1' e1' d2' s2' p2' dp2' e2'
                d1' = if s1 == s1' then Nothing else Just p1
                d2' = if s2 == s2' then Nothing else Just p2
                dp1' = p1' - p1
                dp2' = p2' - p2
                e1' | p1 /= p1' = 0
                    | p2 /= p2' = 1
                    | otherwise = e1 * friction
                e2' | p2 /= p2' = 0
                    | p1 /= p1' = 1
                    | otherwise = e2 * friction


    drawBar :: BarFold -> Drawing PixelRGBA8 ()
    drawBar (BF n d1 s1 p1 dp1 e1 d2 s2 p2 dp2 e2) = do
      -- p1
      withTexture (uniformTexture p1Color) . fill$
        rectangle (V2 x (baseY-yOffset))
                  w (negate h1)

      -- e1
      withTexture (uniformTexture (p1e e2)) . fill$
        rectangle (V2 x (baseY-h1-yOffset))
                  w (dh1-eHeight)

      -- p2
      withTexture (uniformTexture p2Color) . fill$
        rectangle (V2 x (baseY+yOffset))
                  w h2

      -- e2
      withTexture (uniformTexture (p2e e1)) . fill$
        rectangle (V2 x (baseY+h2+yOffset))
                  w (eHeight-dh2)

      withTexture (uniformTexture markerColor)$
        do case d1 of Nothing -> mempty
                      Just _  -> fill$ rectangle (V2 x (baseY-yOffset)) w (-10-barHeight)
           case d2 of Nothing -> mempty
                      Just _  -> fill$ rectangle (V2 x (baseY+yOffset)) w (10+barHeight)

      where
        h1  = fromIntegral p1  * barHeight / p1Max
        dh1 = fromIntegral dp1 * barHeight / p1Max
        h2  = fromIntegral p2  * barHeight / p2Max
        dh2 = fromIntegral dp2 * barHeight / p2Max
        x   = xOffset+n*barWidth
        w   = barWidth + 3

    img =
      do renderDrawing imageWidth imageHeight bgColor $ traverse_ drawBar bars

testGraph :: IO ()
testGraph = runConduitRes$ yield [] .| mapMC (liftIO . gameGraph) .| sinkNull
