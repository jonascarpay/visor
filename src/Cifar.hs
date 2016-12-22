{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}

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
cifarNet = initCNet [ConvS 5 64, ReLUS, PoolS, ConvS 5 64, ReLUS, PoolS] 32 32 [10]

train3C :: ConvNet -> Consumer CifarSample (ResourceT IO) ConvNet
train3C n@(ConvNet l3s l1s) =
  do ms <- await
     case ms of
       Just (CifarSample (ConvSample x y)) ->
         do (_, l3s', l1s', loss) <- train3 l3s l1s x y 1e-2
            liftIO . print $ loss
            train3C (ConvNet l3s' l1s')
       Nothing -> return n

-- TODO: Enforce 0-1 normalization?
rgbToImage :: Volume -> DynamicImage
rgbToImage vol = ImageRGB8 $ generateImage convFn w h
  where
    Z:.3:.h:.w = extent vol
    c x = round $ x * 255
    convFn x y = let r = vol ! ix3 0 y x
                     g = vol ! ix3 1 y x
                     b = vol ! ix3 2 y x
                  in PixelRGB8 (c r) (c g) (c b)

greyScaleToImage :: Monad m => Matrix -> m DynamicImage
greyScaleToImage img = do img' <- lerp img 0 255
                          return . ImageRGB8 $ generateImage (arrLkUp img') w h
  where
    Z:.h:.w = extent img
    arrLkUp a x y = let v = a ! ix2 y x
                     in PixelRGB8 (round v) (round v) (round v)

-- The `take 1` hard codes this to only save images from the first layer
saveWeightImages :: ConvNet -> IO ()
saveWeightImages (ConvNet l3s _) = do ms <- Prelude.traverse splitW $ getWeights l3s
                                      _ <-sequence [ saveFn m li wi | (li, m') <- zip [1..] (take 1 ms), (wi, m) <- zip [1..] m']
                                      return ()
  where
    saveFn m (li::Int) (wi::Int) =
      do m' <- greyScaleToImage m
         savePngImage ("data" </> "cifar" </> show li ++ "_" ++ show wi ++ ".png") m'

imageSink :: IOSink CifarSample
imageSink = go (0 :: Int)
  where go n = do mimg <- await
                  case mimg of
                    Just (CifarSample (ConvSample x y)) ->
                      do liftIO $ savePngImage ("data" </> "cifar" </> show n ++ "_" ++ show (fromLabel y) ++ ".png") (rgbToImage x)
                         go (n+1)
                    Nothing -> return ()