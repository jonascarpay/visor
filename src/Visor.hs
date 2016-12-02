module Visor where

import Network
import Game
import Games.Melee
import Batch
import Util
import Conduit
import Data.Conduit.Zlib
import Data.List
import Data.Serialize
import qualified Data.ByteString   as BS
import Numeric.LinearAlgebra
import System.FilePath
import System.Directory
import Vision.Image
import Vision.Primitive.Shape
import Vision.Primitive

data Visor = Visor { game :: Game
                   , nets :: [Network]
                   }

-- | A visor consists of multiple networks. A network is fed
--   batches. A VBatch is a collection of these batches
--   that can be fed to a visor.
type VBatch = [NetBatch]

fromGame :: Game -> IO Visor
fromGame game =
  do nets <- traverse forFeature (features game)
     return $ Visor game nets
  where
    -- The network for a feature always has one output more than
    -- the feature cardinality. The extra output is used for
    -- the case where the output is undefined
    forFeature :: Feature -> IO Network
    forFeature (Feature _ _ _ (rw,rh) k) = initNet (rw*rh) (k+1) [100] 1 1

consolidate :: [VBatch] -> VBatch
consolidate vis = zipWith NetBatch xStack yStack
  where
    stack :: [Matrix R] -> Matrix R
    stack = fromBlocks . fmap (:[])
    t' = transpose vis
    xs = (fmap.fmap) input  t'
    ys = (fmap.fmap) output t'
    xStack = fmap stack xs
    yStack = fmap stack ys

stack :: NetBatch -> NetBatch -> NetBatch
stack (NetBatch i1 o1) (NetBatch i2 o2) = NetBatch (i1 === i2) (o1 === o2)

genBatch :: Int -> Dataset -> Visor -> IO ()
genBatch n set visor = runConduitRes $ batchSrc .| batchSink
 where
   batchSrc = asSource set .| interpret .| cvb'
   interpret = mapC (toVBatch . features . game $ visor)
   cvb' = awaitForever $ \x -> do stacked <- takeC (n-1) .| foldlC (zipWith stack) x
                                  yield stacked

   dir = "data" </> "batch" </> (title . game $ visor)

   batchSink :: IOSink VBatch
   batchSink = do liftIO $ createDirectoryIfMissing True dir
                  mapC encode .| iterWrite 0

   iterWrite :: Int -> IOSink BS.ByteString
   iterWrite i = do liftIO . putStrLn $ "Loading batch " ++ show i
                    mbs <- await
                    case mbs of
                      Just bs -> do yield bs .| gzip .| sinkFileBS (dir</>show i)
                                    liftIO . putStrLn $ "Wrote batch " ++ show i
                                    iterWrite (i+1)
                      Nothing -> do return ()
                                    liftIO . putStrLn $ "Pipeline exhausted, exiting"

test :: IO ()
test = do v <- fromGame melee
          l <- genBatch 256 dolphin_sets v
          print l

-- | For a given visor, split an image and a set of labels into
--   NetBatches corresponding to each feature.
toVBatch :: [Feature] -> (RGBDelayed, [Maybe Int]) -> VBatch
toVBatch fs (img, lbls) = zipWith NetBatch xs ys
  where
    xs :: [Matrix R]
    xs = extractFeature img <$> fs

    ys :: [Matrix R]
    ys = zipWith toIndexMatrix labelsGrouped (cardinality <$> fs)

    labelsGrouped = matchShape lbls (fmap positions fs)

    matchShape _   []     = []
    matchShape ins (e:es) = let (pre,post) = splitAt (length e) ins
                             in pre : matchShape post es

-- | Extracts samples for some feature from an image
extractFeature :: RGBDelayed -> Feature -> Matrix R
extractFeature img (Feature _ pos (fw,fh) (rx, ry) _) = combine resized
  where
    (Z:.ih:.iw) = shape img

    toArea :: Double -> Double -> Double -> Double -> Int -> Int -> Rect
    toArea cx cy fw fh iw ih = let xRel = cx - fw / 2
                                   yRel = cy - fh / 2
                                   wRel = fw
                                   hRel = fh
                                   x = round $ fromIntegral iw * xRel
                                   y = round $ fromIntegral ih * yRel
                                   w = round $ fromIntegral iw * wRel
                                   h = round $ fromIntegral ih * hRel
                                in Rect x y w h

    cropAreas :: [Rect]
    cropAreas = fmap (\(cx, cy) -> toArea cx cy fw fh iw ih) pos
    crops :: [RGBDelayed]
    crops = fmap (`crop` img) cropAreas
    resized :: [RGBDelayed]
    resized = fmap (resize Bilinear $ ix2 ry rx) crops
    combine :: [RGBDelayed] -> Matrix R
    combine = fromRows . fmap (imageToVector . convert)
