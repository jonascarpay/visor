module Games.Melee.Graphic where

import Games.Melee
import Types
import Lib

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

pad :: [a] -> Int -> a -> [a]
pad xs n x = xs ++ replicate (n - length xs) x

killPctsP1 :: MeleeGame -> [Int]
killPctsP1 (Ingame (PlayerState s p) _ _ _ : h@(Ingame (PlayerState s' _) _ _ _) : gs)
  | s == s' = killPctsP1 (h:gs)
  | otherwise = p:killPctsP1 (h:gs)
killPctsP1 [Ingame (PlayerState _ p) q1 _ _, Win _ q]
  | q1 == q = []
  | otherwise = [p]

killPctsP2 :: MeleeGame -> [Int]
killPctsP2 (Ingame _ _ (PlayerState s p) _ : h@(Ingame _ _ (PlayerState s' _) _) : gs)
  | s == s' = killPctsP2 (h:gs)
  | otherwise = p:killPctsP2 (h:gs)
killPctsP2 [Ingame _ _ (PlayerState _ p) q2, Win _ q]
  | q2 == q = []
  | otherwise = [p]

punishP1 :: MeleeGame -> [Int]
punishP1 game = go 0 game
  where
    go punish (Ingame p1 _ (PlayerState s2 p2) _ : h@(Ingame p1' _ (PlayerState s2' p2') _) : gs)
      | p1 == p1' && s2' == s2 = go (punish + p2' - p2) (h:gs)
      | otherwise = punish:go 0 (h:gs)
    go _ _ = []

punishP2 :: MeleeGame -> [Int]
punishP2 game = go 0 game
  where
    go punish (Ingame (PlayerState s1 p1) _ p2 _ : h@(Ingame (PlayerState s1' p1') _ p2' _) : gs)
      | p2 == p2' && s1' == s1 = go (punish + p1' - p1) (h:gs)
      | otherwise = punish:go 0 (h:gs)
    go _ _ = []



averageInt :: [Int] -> Int
averageInt [] = 0
averageInt xs = sum xs `div` length xs

averageF :: [Int] -> Float
averageF [] = 0
averageF xs = fromIntegral (sum xs) / fromIntegral (length xs)

gameGraph :: MeleeGame -> IO ()
gameGraph full'@(Win _ wq:game') =
  do efont <- loadFontFile "data/fonts/39335_UniversCondensed.ttf"
     let font = case efont of
                  Left err   -> error err
                  Right font -> font

     writePng "output.png" (img font)
  where
    game = reverse game'
    full = reverse full'
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
    baseY      = 340
    barWidth   = graphWidth / fromIntegral frames
    barHeight  = 60

    p1Color = PixelRGBA8 100 30 20 255
    p2Color = PixelRGBA8 20 30 100 255

    bgColor     = PixelRGBA8 10 10 10 255
    markerColor = PixelRGBA8 255 250 230 255

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

    drawBar font bf = do
      -- p1
      withTexture (uniformTexture p1Color) . fill$
        rectangle (V2 x (baseY-yOffset))
                  w (negate h1)

      -- p2
      withTexture (uniformTexture p2Color) . fill$
        rectangle (V2 x (baseY+yOffset))
                  w h2

      withTexture (uniformTexture markerColor)$
        do case p1death bf of
             Nothing -> mempty
             Just p  -> do fill$ rectangle (V2 x (baseY-yOffset)) (w+1) (-10-barHeight)
                           printTextAt font (PointSize 12) (V2 (x+6) (baseY-yOffset-3-scalep1 p)) (show p)
           case p2death bf of
             Nothing -> mempty
             Just p  -> do fill$ rectangle (V2 x (baseY+yOffset)) (w+1) (10+barHeight)
                           printTextAt font (PointSize 12) (V2 (x+6) (baseY+yOffset+15+scalep2 p)) (show p)

      where
        scalep1 p = fromIntegral p * barHeight / p1Max
        scalep2 p = fromIntegral p * barHeight / p2Max
        h1  = scalep1$ p1p bf
        h2  = scalep2$ p2p bf
        x   = floorF$ xOffset + n bf * barWidth
        w   = ceilF barWidth

    img font =
      renderDrawing imageWidth imageHeight bgColor$
        do traverse_ (drawBar font) bars
           withTexture (uniformTexture markerColor)$ do
              printTextAt font (PointSize 12) (V2 400 (fromIntegral$ imageHeight - 20))
                "github.com/jonascarpay/visor"

              let size  = 18
                  lx    = 120
                  col1x = 350
                  col2x = 420
                  printText = printTextAt font (PointSize size)

              printTextAt font (PointSize 24) (V2 205 50) "MATCH REPORT"

              printText (V2 col1x 95) ("P" ++ show p1q)
              printText (V2 col2x 95) ("P" ++ show p2q)

              printText (V2 lx    135) "Life expectancy"
              printText (V2 col1x 135)$ show . averageInt $ killPctsP1 full
              printText (V2 col2x 135)$ show . averageInt $ killPctsP2 full

              printText (V2 lx    165) "Average punish"
              printText (V2 col1x 165)$ take 4 . show . averageF $ punishP1 full
              printText (V2 col2x 165)$ take 4 . show . averageF $ punishP2 full

              printText (V2 lx    195) "Biggest punish"
              printText (V2 col1x 195)$ show . maximum $ punishP1 full
              printText (V2 col2x 195)$ show . maximum $ punishP2 full

              printText (V2 lx    225) "Conversion rate"
              printText (V2 col1x 225)$ take 4 . show . (*100) $ fromIntegral (length.killPctsP2$full) / fromIntegral (length.punishP1$full)
              printText (V2 col2x 225)$ take 4 . show . (*100) $ fromIntegral (length.killPctsP1$full) / fromIntegral (length.punishP2$full)

