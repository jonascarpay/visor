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
import Codec.Picture
import Data.Array.Repa as R hiding ((++))

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
cifarNet = initCNet [ConvS 11 32, ReLUS, ConvS 11 16, ReLUS, PoolS] 32 32 10

train3C :: ConvNet -> IOSink CifarSample
train3C (ConvNet l3s l1s) =
  do Just (CifarSample (ConvSample x y)) <- await
     (_, l3s', l1s', loss) <- train3 l3s l1s x y 1e-3
     liftIO . print $ loss
     train3C (ConvNet l3s' l1s')

toImage :: Volume -> DynamicImage
toImage vol = ImageRGB8 $ generateImage convFn w h
  where
    Z:.3:.h:.w = extent vol
    c x = round $ x * 255
    convFn x y = let r = vol ! ix3 0 y x
                     g = vol ! ix3 1 y x
                     b = vol ! ix3 2 y x
                  in PixelRGB8 (c r) (c g) (c b)

imageSink :: IOSink CifarSample
imageSink = go (0 :: Int)
  where go n = do mimg <- await
                  case mimg of
                    Just (CifarSample (ConvSample x y)) ->
                      do liftIO $ savePngImage ("data" </> "cifar" </> show n ++ "_" ++ show (fromLabel y) ++ ".png") (toImage x)
                         go (n+1)
                    Nothing -> return ()
