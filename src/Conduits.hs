{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}

module Conduits ( module Conduit
                , module Conduits
                ) where

import Game
import Util
import Batch
import Visor
import Vision.Image
import Vision.Image.Storage.DevIL
import System.Directory
import System.FilePath
import System.Posix
import Data.Serialize
import qualified Data.ByteString as BS
import Data.Conduit.Zlib
import Conduit
import qualified Data.Conduit.Combinators as CC
import Control.Monad
import Control.DeepSeq

-- TODO: Write Conduit typeclass for source/sink

-- | A source conduit that yields successive images from a dataset.
datasetSource :: Dataset -> IOSrc LabeledImage
datasetSource (Dataset root lFn rect wig dis) = paths .| pair
  where
    paths       = sourceDirectoryDeep True root .| filterC ((==".png") . takeExtension)
    imgSource p = do liftIO . putStrLn $ "Loading " ++ p
                     bs <- sourceFileBS p .| foldC
                     yield bs
    pair        = awaitForever $ \p -> imgSource p .| toRGB .| mapC (,lFn p)
    toRGB       = mapMC $ \bs -> loadImage bs rect wig dis

-- | Collect n LabeledImages and consolidate them into a single VBatch.
parseLabeledImage :: Game -> Int -> IOConduit LabeledImage VBatch
parseLabeledImage game n = interpret .| gatherC
  where
   interpret = mapC (toVBatch . features $ game)
   gatherC = takeC n .| foldl1' (zipWith stack) >>= yield

featureSink :: Game -> IOSink LabeledImage
featureSink (Game _ fs) = go (0 :: Int)
  where go n = do mli <- await
                  case mli of
                    Nothing -> return ()
                    Just (img, lbls) ->
                      let imgsD = fs >>= snd . extractFeature' img
                          imgs  = fmap compute imgsD
                          lbls'  = zip [(1::Int)..] lbls
                          fname (i, Just lbl) = show n ++ "_" ++ show i ++ "_" ++ show lbl ++ ".png"
                          fname (i, Nothing)  = show n ++ "_" ++ show i ++ "__.png"
                          dir   = "data" </> "features"
                          save' :: RGB -> (Int, Maybe Int) -> IO (Maybe StorageError)
                          save' i f = save PNG (dir</>fname f) i
                          saves :: [IO (Maybe StorageError)]
                          saves = zipWith save' imgs lbls'
                       in do liftIO $ do createDirectoryIfMissing True dir
                                         sequence_ saves
                             go (n+1)

labelSink :: ([Maybe Int] -> String) -> IOSink LabeledImage
labelSink delabel = go (0::Int)
  where go :: Int -> IOSink LabeledImage
        go n = do mli <- await
                  case mli of
                    Nothing -> return ()
                    Just (img, lbls) ->
                      let filename = show n ++ "_" ++ delabel lbls ++ ".png"
                          dir = "data" </> "labeled"
                       in do liftIO $ do createDirectoryIfMissing True dir
                                         save PNG (dir</>filename) (convert img :: RGB)
                             go (n+1)

-- | Write VBatches to the directory specified
batchSink :: String -> IOSink VBatch
batchSink dirname = do liftIO $ createDirectoryIfMissing True dir
                       mapC encode .| iterWrite 0
  where
    dir = "data" </> "batch" </> dirname
    iterWrite :: Int -> IOSink BS.ByteString
    iterWrite i = do mbs <- await
                     case mbs of
                       Just bs -> do yield bs .| compress 9 defaultWindowBits .| sinkFileBS (dir</>dirname ++ show i ++ ".vbatch")
                                     liftIO . putStrLn $ "Wrote batch " ++ show i
                                     iterWrite (i+1)
                       Nothing -> do return ()
                                     liftIO . putStrLn $ "Pipeline exhausted, exiting"

-- | Streams batches from the directory specified
batchSource :: String -> IOSrc VBatch
batchSource dirname =
      sourceDirectory ("data"</>"batch"</>dirname)
   .| filterC ((==".vbatch") . takeExtension)
   .| sourceFileBS'
   .| mapC decode
   .| eitherC

-- | Convert a dataset into VBatches and write them to disk
genBatch :: Int -> Dataset -> Game -> IO ()
genBatch n set game = runConduitRes $ datasetSource set
                                   .| parseLabeledImage game n
                                   .| batchSink (title game)

-- | Auxiliary function to open and decompress large bytestrings. Can someone please
--   tell me why sourceFileBS only produces 32kB chunks?
sourceFileBS' :: IOConduit FilePath BS.ByteString
sourceFileBS' = awaitForever $ \p -> do liftIO . putStrLn $ "Opening " ++ p
                                        bs <- sourceFileBS p
                                                .| decompress defaultWindowBits
                                                .| foldC
                                        yield bs

-- | Conduit that prints out Left values as error messages, while extracting Right values
eitherC :: IOConduit (Either String o) o
eitherC = awaitForever $ either (liftIO . putStrLn . ("Left: "++)) yield

-- | Loads a visor with the given name, or initializes a new one if none exists.
visorSource :: Game -> IOSrc Visor
visorSource g@(Game vName _) =
  do b <- liftIO $ fileExist p
     if b then yield p .| sourceFileBS' .| mapC decode .| eitherC
          else liftIO (fromGame g) >>= yield
    where
      p = "data" </> "visor" </> vName ++ ".visor"

-- TODO: Better name
genVisor :: Game -> IO (Maybe Visor)
genVisor g = runConduitRes $ visorSource g .| headC

-- | Writes a visor
visorSink :: IOSink Visor
visorSink = awaitForever $
  \ v@(Visor vName _) -> yield v
                      .| mapC encode
                      .| compress 9 defaultWindowBits
                      .| write vName
  where
    dir = "data" </> "visor"
    write vName = do liftIO $ createDirectoryIfMissing True dir
                     sinkFileBS (dir </> vName ++".visor")

-- | Trains a visor
trainC :: Visor -> IOConduit VBatch Visor
trainC v = do mvb <- await
              case mvb of
                Nothing -> return ()
                Just vb -> let (ns', l)   = vTrain v vb
                               v'         = v {nets = ns'}
                               acc        = vAccuracy v' vb
                               lossString = unwords $ fmap (take 9 . show) l
                               accString  = unwords $ fmap ((++"%") . take 4 . show) acc
                               output     = "\r" ++ accString ++ "\t" ++ lossString
                            in do liftIO . putStrLn $ output
                                  yield v'
                                  trainC v'

foldl1' :: (NFData b, Monad m) => (b -> b -> b) -> ConduitM b o m b
foldl1' f = do Just x <- CC.foldl1 f
               return x

loopC :: Monad m => m a -> m b
loopC c = c >> loopC c

replicateCC :: Monad m => Int -> ConduitM o o m ()
replicateCC n = awaitForever $ \x -> replicateM n (yield x)
