module IO
  ( readShot
  , datasetPathSource
  , shuffleC
  ) where

import Types
import Lib
import Static.Image
import Conduit
import System.FilePath
import System.Random.Shuffle

readShot :: FilePath -> IO (Screenshot a)
readShot fp = do ebmp <- readRaw fp
                 case ebmp of
                   Left err -> error err
                   Right bmp -> return (Screenshot bmp)

datasetPathSource :: Dataset a -> RTSource FilePath
datasetPathSource set = sourceDirectoryDeep True (rootDir set) .| filterC ((== ".bmp") . takeExtension)

-- | Drain a source of its elements, and yield them in a random order
shuffleC :: RTConduit a a
shuffleC = do ls <- sinkList
              ls' <- liftIO$ shuffleM ls
              yieldMany ls'
