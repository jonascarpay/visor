module IO
  ( readShot
  , datasetPathSource
  ) where

import Types
import Lib
import Static.Image
import Conduit
import System.FilePath

readShot :: FilePath -> IO (Screenshot a)
readShot fp = do ebmp <- readRaw fp
                 case ebmp of
                   Left err -> error err
                   Right bmp -> return (Screenshot bmp)

datasetPathSource :: Dataset a -> RTSource FilePath
datasetPathSource set = sourceDirectoryDeep True (rootDir set) .| filterC ((== ".bmp") . takeExtension)
