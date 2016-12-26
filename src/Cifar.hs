{-# LANGUAGE ScopedTypeVariables #-}

module Cifar where

import Util
import ConvNet
import Volume
import Label
import Conduit
import Data.Conduit.Cereal
import System.FilePath
import Data.Serialize
import Data.Word
import Control.Monad

newtype CifarSample = CifarSample {getSample :: ConvSample} deriving (Eq, Show)

instance Serialize CifarSample where
  put = error "Error writing CIFAR sample"
  get = do label ::  Word8  <- get
           bytes :: [Word8] <- replicateM (3*1024) get
           return . CifarSample $ ConvSample (toCifarVolume bytes) [toLabel label]

sourceCifar :: IOSrc ConvSample
sourceCifar = sourceDirectoryDeep True ("data" </> "cifar")
           .| filterC ((==".bin") . takeExtension)
           .| awaitForever sourceFileBS
           .| conduitGet2 (get :: Get CifarSample)
           .| mapC getSample

cifarNet :: ConvNet
cifarNet = initCNet [ConvS 5 64, ReLUS, PoolS, ConvS 5 64, ReLUS, PoolS] 32 32 [10]

