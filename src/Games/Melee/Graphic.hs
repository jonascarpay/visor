module Games.Melee.Graphic where

import Games.Melee
import Types

import Data.Foldable (traverse_)
import Conduit

import Graphics.Rasterific
import Graphics.Rasterific.Texture
import Graphics.Text.TrueType (loadFontFile)
import Codec.Picture (PixelRGBA8( .. ), writePng)

data BarFold = BF
  { n :: Float
  , p1death :: Maybe Int
  , p1s :: Int
  , p1p :: Int
  , p1dp :: Int
  , p2death :: Maybe Int
  , p2s :: Int
  , p2p :: Int
  , p2dp :: Int
  }

type MeleeGame = [Melee]

gameGraph :: MeleeGame -> IO ()
gameGraph (w@(Win (PlayerState ws wp) wq):game') =
  do efont <- loadFontFile "data/fonts/A-OTF-FolkPro-Regular.otf"
     let font = case efont of
                  Left err   -> error err
                  Right font -> font

     writePng "output.png" (img font)
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
        where b' = BF 0 Nothing 4 0 0 Nothing 4 0 0

    foldbars (Just (BF n _ s1 p1 _ _ s2 p2 _)) [] =
      [if wq == p1q
          then BF (n+1) Nothing   s1 p1 0 (Just p2) s2 p2 0
          else BF (n+1) (Just p1) s1 p1 0 Nothing   s2 p2 0
      ]

    foldbars
      (Just (BF n _ s1 p1 _ _ s2 p2 _))
      (Ingame (PlayerState s1' p1') _ (PlayerState s2' p2') _:t) =
        b' : foldbars (Just b') t
          where b' = BF (n+1) d1' s1' p1' dp1' d2' s2' p2' dp2'
                d1' = if s1 == s1' then Nothing else Just p1
                d2' = if s2 == s2' then Nothing else Just p2
                dp1' = p1' - p1
                dp2' = p2' - p2

    drawBar :: BarFold -> Drawing PixelRGBA8 ()
    drawBar (BF n d1 s1 p1 dp1 d2 s2 p2 dp2) = do
      -- p1
      withTexture (uniformTexture p1Color) . fill$
        rectangle (V2 x (baseY-yOffset))
                  w (negate h1)

      -- p2
      withTexture (uniformTexture p2Color) . fill$
        rectangle (V2 x (baseY+yOffset))
                  w h2

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

    img font =
      do renderDrawing imageWidth imageHeight bgColor $ traverse_ drawBar bars

testGraph :: IO ()
testGraph = runConduitRes$ yield [] .| mapMC (liftIO . gameGraph) .| sinkNull
