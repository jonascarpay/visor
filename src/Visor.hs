{-# LANGUAGE DeriveGeneric #-}

module Visor where

import Label
import ConvNet
import Game
import Images
import Control.Monad
import Data.Serialize
import GHC.Generics (Generic)
import qualified Data.ByteString as BS
import System.Directory
import System.FilePath
import Codec.Picture

newtype Visor = Visor [ConvNet] deriving (Show, Generic)
instance Serialize Visor

-- | Specialized training function for monadic folding. The List of doubles
--   accumulates losses.
train' :: Monad m => Double -> (ConvNet, [[Double]]) -> ConvSample -> m (ConvNet, [[Double]])
train' α (ConvNet l3s l1ss, losses) (ConvSample i o) =
  do (_, l3s', l1ss', loss) <- train3 l3s l1ss i o α
     return (ConvNet l3s' l1ss', loss:losses)

trainVisor :: Monad m => Visor -> VisorSample -> m (Visor, [[[Double]]])
trainVisor (Visor nets) css = do zs <- z
                                 let (nets, losses) = unzip zs
                                 return (Visor nets, losses)
  where
    f :: Monad m => ConvNet -> ConvSampleSequence -> m (ConvNet, [[Double]])
    f net cs = foldM (train' 5e-3) (net, []) cs
    z = zipWithM f nets css

gameVisor :: Game -> Visor
gameVisor (Game _ ws) = Visor (fmap widgetNet ws)
  where widgetNet (Widget res _ _ cs spec) = initCNet spec res res ((+1) <$> cs) -- +1 to account for Indeterminate in cardinality

feedVisor :: Monad m => Visor -> [[Palette]] -> Double -> m [[[Label]]]
feedVisor (Visor nets) imgss t = zipWithM (feedMany t) nets imgss
  where
    feedMany :: Monad m => Double -> ConvNet -> [Palette] -> m [[Label]]
    feedMany t net imgs = mapM (feed' t net) imgs
    feed' t net img = do vol <- toVolumeP img
                         feedThresholded t net vol

loadVisor :: FilePath -> Game -> IO Visor
loadVisor fp game = do exist <- doesFileExist fp
                       if exist then do bs <- BS.readFile fp
                                        let Right v = decode bs
                                        return v
                                else return $ gameVisor game

saveVisor :: Serialize a => FilePath -> a -> IO ()
saveVisor fp visor = do createDirectoryIfMissing True (takeDirectory fp)
                        BS.writeFile fp (encode visor)
