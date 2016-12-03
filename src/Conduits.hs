{-# LANGUAGE TupleSections #-}

module Conduits ( module Conduit
                , module Conduits
                ) where

import Game
import Util
import Batch
import Visor
import System.Directory
import System.FilePath
import System.Posix
import Data.Serialize
import qualified Data.ByteString as BS
import Data.Conduit.Zlib
import Conduit

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
   gatherC = awaitForever $ \x -> do stacked <- takeC (n-1) .| foldlC (zipWith stack) x
                                     yield stacked

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
batchSource dirname = sourceDirectory ("data"</>"batch"</>dirname)
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
visorSource g@(Game vName _ _) =
  do b <- liftIO $ fileExist p
     if b then yield p .| sourceFileBS' .| mapC decode .| eitherC
          else liftIO (fromGame g) >>= yield
    where
      p = "data" </> "visor" </> vName ++ ".visor"

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
                Just vb -> let (ns', l) = vTrain v vb
                               v'       = v {nets = ns'}
                            in do liftIO . putStrLn $ "Loss: " ++ show l
                                  yield v'
                                  trainC v'
