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
import Visor
import Conduit
import Codec.Picture
import Codec.Picture.Extra
import Data.Serialize
import qualified Data.ByteString as BS
import System.Directory
import System.FilePath
import System.Random
import System.Random.Shuffle

-- | Loads an image and applies desired transformations
datasetSource :: Bool -- ^ Whether or not the list should be in shuffled order.
                      --   Note that this will build a large cache of filenames
                      --   and might be slow, especially for large datasets.
              -> Dataset
              -> IOSrc (Palette, [[WidgetLabel]])
datasetSource shuf (Dataset root lblFn (Rect x y w h) wig dist) =
       sourceDirectoryDeep True root
    .| filterC ((/='.') . head . takeFileName)
    .| (if shuf then shuffleConduit .| loadImageC else loadImageC)
  where
    loadImageC :: IOConduit FilePath (Palette, [[WidgetLabel]])
    loadImageC = awaitForever$
      \fp -> do liftIO.putStrLn$ "Loading " ++ fp
                o <- liftIO $ loadImage fp
                yield o

    loadImage :: FilePath -> IO (Palette, [[WidgetLabel]])
    loadImage fp = do Right img' <- readImage fp
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

imageSink :: IOSink ConvSample
imageSink = go (0 :: Int)
  where go n = do mimg <- await
                  case mimg of
                    Just (ConvSample x y) ->
                      do liftIO $ savePngImage ("data" </> "cifar" </> show n ++ "_" ++ show y ++ ".png") (rgbToImage x)
                         go (n+1)
                    Nothing -> return ()

batchSink :: Int -> IOSink VisorSample
batchSink batchSize = go (0::Int)
  where
    go n = do ms <- takeC batchSize .| sinkList
              case ms of
                [] -> return ()
                s  -> do liftIO$ do createDirectoryIfMissing True ("data"</>"batch")
                                    BS.writeFile ("data"</>"batch"</>show n ++ ".vbatch") (encode s)
                         go (n+1)

unpackBatch :: Monad m => Conduit VisorSample m [[(Palette,WidgetLabel)]]
unpackBatch = mapMC (traverse.traverse$convert)
  where
    convert :: Monad m => ConvSample -> m (Palette, WidgetLabel)
    convert (ConvSample vol ls) = do ImageRGB8 img <- rgbNormalizeToImage vol
                                     return (img, ls)

batchSource :: IOSrc VisorSample
batchSource = sourceDirectoryDeep True ("data"</>"batch") .| awaitForever load
  where load fp = do bs <- liftIO$ BS.readFile fp
                     case decode bs of
                       Left err -> liftIO$ putStrLn err
                       Right (s :: [VisorSample])  -> yieldMany s

parseSink :: IOSink [[(Palette, WidgetLabel)]]
parseSink = go (0 :: Int)
  where
    dir = "data"</>"out"
    prep :: Show a => a -> ShowS
    prep x s = show x ++ "_" ++ s
    ix xs = zip xs [(1::Int)..]
    go n = do ms <- await
              case ms of
                Just s  -> do liftIO . sequence_ $ [ writePng (dir</>(prep n . prep i . prep j . prep l $ ".png")) img
                                                   | (s',i) <- ix s
                                                   , ((img,l),j) <- ix s'
                                                   ]
                              liftIO.putStrLn$ "Writing sample " ++ show n
                              go (n+1)
                Nothing -> return ()

loopC :: Monad m => m a -> m b
loopC c = c >> loopC c

trainC :: ConvNet -> Consumer ConvSample (ResourceT IO) ConvNet
trainC n@(ConvNet l3s cs) =
  do ms <- await
     case ms of
       Just (ConvSample x y) ->
         do (_, l3s', loss) <- train3 l3s x cs y 1e-2
            liftIO . print $ loss
            trainC (ConvNet l3s' cs)
       Nothing -> return n

gameSource :: Game -> Dataset -> Bool -> IOSrc VisorSample
gameSource game set shuf = datasetSource shuf set .| mapC (toSamples game)

trainVisorC :: Visor -> Consumer VisorSample (ResourceT IO) Visor
trainVisorC v = go v (0::Int)
  where
    go v n =
      do ms <- await
         case ms of
           Just s -> do (v', ds) <- trainVisor v s
                        liftIO . putStrLn . (++ ('\t':show n)) . printLosses $ ds
                        go v' (n+1)
           Nothing -> return v

labelC :: Monad m => Visor -> Game -> Double -> Conduit Palette m [[WidgetLabel]]
labelC v g t = mapC (extractWidgets g)
            .| mapMC (\img -> feedVisor v img t)

watchC :: Monad m => Visor -> Game -> Double -> Conduit Palette m [[(Palette,WidgetLabel)]]
watchC v g t = mapC (extractWidgets g)
            .| mapMC (\img -> (zipWith zip img) <$> feedVisor v img t)

-- | A conduit that drains all elements, shuffles them, and then
--   yields those elements. Note that this cannot be used on
--   infinite sources
shuffleConduit :: IOConduit a a
shuffleConduit = do elems <- sinkList
                    let n = length elems
                    liftIO.putStrLn$ "Shuffling " ++ show n ++ " elements"
                    seed <- liftIO randomIO
                    yieldMany $ shuffle' elems n (mkStdGen seed)
