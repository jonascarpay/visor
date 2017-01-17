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
import Data.Serialize
import qualified Data.ByteString as BS
import System.Directory
import System.FilePath
import System.Random
import System.Random.Shuffle
import Control.Monad.Trans.State.Strict

-- | Loads an image and applies desired transformations
datasetSource :: GameState a
              => Bool -- ^ Whether or not the list should be in shuffled order.
                      --   Note that this will build a large cache of filenames
                      --   and might be slow, especially for large datasets.
              -> Dataset a
              -> IOSrc (LabeledImage a)
datasetSource shuf set =
  filePathSource set shuf .| mapMC (liftIO . loadImage set)

filePathSource (Dataset root _ _ _ _) shuf =
  if shuf then source .| shuffleConduit
          else source
  where source = sourceDirectoryDeep True root
                  .| filterC ((/='.') . head . takeFileName)

datasetSink :: IOSink (LabeledImage a)
datasetSink = go (0 :: Int)
  where go n = do mimg <- await
                  case mimg of
                    Just (LabeledImage (img, ls)) ->
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

unpackBatch :: Monad m => Conduit VisorSample m (LabeledWidgets a)
unpackBatch = mapMC (traverse.traverse$convert) .| mapC LabeledWidgets
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

parseSink :: IOSink (LabeledWidgets a)
parseSink = go (0 :: Int)
  where
    dir = "data"</>"out"
    prep :: Show a => a -> ShowS
    prep x s = show x ++ "_" ++ s
    ix xs = zip xs [(1::Int)..]
    go n = do ms <- await
              case ms of
                Just (LabeledWidgets s)
                  -> do liftIO . sequence_ $ [ writePng (dir</>(prep n . prep i . prep j . prep l $ ".png")) img
                                             | (s',i) <- ix s
                                             , ((img,l),j) <- ix s'
                                             ]
                        liftIO.putStrLn$ "Writing sample " ++ show n
                        go (n+1)
                Nothing -> return ()

loopC :: Monad m => m a -> m b
loopC c = c >> loopC c

gameSource :: GameState a => Dataset a -> Bool -> IOSrc VisorSample
gameSource set shuf = datasetSource shuf set .| mapC toSamples

trainVisorC :: GameState a => Visor a -> Consumer VisorSample (ResourceT IO) (Visor a)
trainVisorC v = go (initVisorTrainState v) (0::Int)
  where
    go v n =
      do ms <- await
         case ms of
           Just s -> do let (ds, v') = runState (trainVisor s) v
                        liftIO . putStrLn . (++ ('\t':show n)) . printLosses $ ds
                        go v' (n+1)
           Nothing -> return $ toVisor v

-- | A conduit that drains all elements, shuffles them, and then
--   yields those elements. Note that this cannot be used on
--   infinite sources
shuffleConduit :: IOConduit a a
shuffleConduit = do elems <- sinkList
                    let n = length elems
                    liftIO.putStrLn$ "Shuffling " ++ show n ++ " elements"
                    seed <- liftIO randomIO
                    yieldMany $ shuffle' elems n (mkStdGen seed)
