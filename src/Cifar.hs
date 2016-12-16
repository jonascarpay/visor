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
           return . CifarSample $ ConvSample (toCifarVolume bytes) (toLabel label)

sourceCifar :: IOSrc CifarSample
sourceCifar = sourceDirectoryDeep True ("data" </> "cifar")
           .| filterC ((==".bin") . takeExtension)
           .| awaitForever sourceFileBS
           .| conduitGet2 (get :: Get CifarSample)

cifarNet :: ConvNet
cifarNet = initCNet [ConvS 11 32, ReLUS, ConvS 11 32, ReLUS, PoolS] 32 32 10

train3C :: ConvNet -> IOSink CifarSample
train3C (ConvNet l3s l1s) =
  do Just (CifarSample (ConvSample x y)) <- await
     (_, l3s', l1s', loss) <- train3 l3s l1s x y 1e-3
     liftIO . print $ loss
     train3C (ConvNet l3s' l1s')
