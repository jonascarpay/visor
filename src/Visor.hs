{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Visor where

import ConvNet
import Game
import Images
import Volume
import Control.Monad
import Data.Serialize (Serialize, decode, encode)
import GHC.Generics (Generic)
import qualified Data.ByteString as BS
import System.Directory
import System.FilePath
import Codec.Picture
import Control.Monad.Trans.State.Strict

newtype Visor = Visor [ConvNet] deriving (Show, Generic)
instance Serialize Visor

type VisorTrainer = State [TrainState]

trainVisor :: VisorSample -> VisorTrainer [[LossVector]]
trainVisor s = do nets  <- get
                  let trainers :: [Trainer [LossVector]] = mapM train <$> s
                      (losses, nets') = unzip$ zipWith runState trainers nets
                  put nets'
                  return losses

initVisorTrainer :: Visor -> [TrainState]

gameVisor :: Game -> Visor
gameVisor (Game _ ws) = Visor (fmap widgetNet ws)
  where widgetNet (Widget res _ _ cs spec) = initCNet spec res res ((+1) <$> cs) -- +1 to account for Indeterminate in cardinality

feedVisor :: Monad m => Visor -> [[Palette]] -> Double -> m [[WidgetLabel]]
feedVisor (Visor nets) imgss t = zipWithM (feedMany t) nets imgss
  where
    feedMany :: Monad m => Double -> ConvNet -> [Palette] -> m [WidgetLabel]
    feedMany t net imgs = mapM (feed' t net) imgs
    feed' t net img = do vol <- toVolumeP img
                         feedThresholded t net vol

feedVisorFast :: Monad m => Visor -> [[Volume]] -> Double -> m [[WidgetLabel]]
feedVisorFast (Visor nets) volss t = do zipWithM (feedMany t) nets volss
  where
    feedMany :: Monad m => Double -> ConvNet -> [Volume] -> m [WidgetLabel]
    feedMany t net vols = mapM (feedThresholded t net) vols


loadVisor :: FilePath -> Game -> IO Visor
loadVisor fp game = do exist <- doesFileExist fp
                       if exist then do bs <- BS.readFile fp
                                        putStrLn $ "Loading " ++ fp
                                        let Right v = decode bs
                                        return v

                                else do putStrLn $ "No visor found at " ++ fp ++", initializing new visor for game " ++ title game
                                        return $ gameVisor game

saveVisor :: Serialize a => FilePath -> a -> IO ()
saveVisor fp visor = do createDirectoryIfMissing True (takeDirectory fp)
                        BS.writeFile fp (encode visor)

-- The `take 1` hard codes this to only save images from the first layer
saveWeightImages :: Visor -> IO ()
saveWeightImages (Visor [ConvNet l3s _]) = do ms <- Prelude.traverse splitW $ getWeights l3s
                                              _  <- sequence [ saveFn m li wi | (li, m') <- zip [1..] (take 1 ms), (wi, m) <- zip [1..] m']
                                              return ()
  where
    saveFn m (li::Int) (wi::Int) =
      do m' <- rgbNormalizeToImage m
         createDirectoryIfMissing True ("data"</>"weights")
         savePngImage ("data" </> "weights" </> show li ++ "_" ++ show wi ++ ".png") m'

saveWeightImages _ = error "Saving weight images not yet supported for multi-widget visors"

