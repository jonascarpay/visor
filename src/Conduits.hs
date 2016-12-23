{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}

module Conduits ( module Conduit
                , module Conduits
                ) where

import ConvNet
import Game
import Images
import Util
import Conduit
import Codec.Picture
import Codec.Picture.Extra
import System.Random
import System.Directory
import System.FilePath

-- | Loads an image and applies desired transformations
datasetSource :: Dataset
              -> IOSrc (Palette, [[WidgetLabel]])
datasetSource (Dataset root lblFn (Rect x y w h) wig dist) =
  sourceDirectoryDeep True root .| filterC ((/='.') . head . takeFileName) .| loadImageC
  where
    loadImageC :: IOConduit FilePath (Palette, [[WidgetLabel]])
    loadImageC = awaitForever$
      \fp -> do o <- liftIO $ loadImage fp
                yield o

    loadImage :: FilePath -> IO (Palette, [[WidgetLabel]])
    loadImage fp = do putStrLn fp
                      Right img' <- readImage fp
                      dx <- randomRIO (0, wig)
                      dy <- randomRIO (0, wig)
                      dw <- randomRIO (0, wig)
                      dh <- randomRIO (0, wig)
                      dr :: Double <- randomRIO (0.9, 1.1)
                      dg :: Double <- randomRIO (0.9, 1.1)
                      db :: Double <- randomRIO (0.9, 1.1)
                      let img = convertRGB8 img'
                          imgCropped = crop (x+dx) (y+dy) (w-dx-dw) (h-dy-dh) img
                          scaleMax x c = round $ min 255 $ fromIntegral x * c
                          distortColor (PixelRGB8 r g b) = PixelRGB8 (scaleMax r dr) (scaleMax g dg) (scaleMax b db)
                          distorted = pixelMap distortColor imgCropped
                      return (if dist then distorted else imgCropped, lblFn fp)

datasetSink :: IOSink (Palette, [[WidgetLabel]])
datasetSink = go (0 :: Int)
  where go n = do mimg <- await
                  case mimg of
                    Just (img, ls) ->
                      do let dir = "data" </> "out"
                             filename = show n ++ show ls ++ ".png"
                         liftIO $ createDirectoryIfMissing True dir
                         liftIO $ writePng (dir</>filename) img
                         go (n+1)
                    Nothing -> return ()

parseSink :: Game -> IOSink (Palette, [[WidgetLabel]])
parseSink game = go (0 :: Int)
  where go n = do mimg <- await
                  case mimg of
                    Just img ->
                      do let labeledWidgets = concat $ extractWidgets game img
                             dir = "data"</>"out"
                             saveWidget (widget, label) = writePng (dir</>show n ++ "_" ++ show label ++ ".png") widget
                         liftIO $ mapM_ saveWidget labeledWidgets
                         go (n+1)
                    Nothing -> return ()

loopC :: Monad m => m a -> m b
loopC c = c >> loopC c

trainC :: ConvNet -> Consumer ConvSample (ResourceT IO) ConvNet
trainC n@(ConvNet l3s l1s) =
  do ms <- await
     case ms of
       Just (ConvSample x y) ->
         do (_, l3s', l1s', loss) <- train3 l3s l1s x y 1e-2
            liftIO . print $ loss
            trainC (ConvNet l3s' l1s')
       Nothing -> return n

