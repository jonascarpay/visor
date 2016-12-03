module Util where

import Numeric.LinearAlgebra
import Data.Word
import Data.Serialize
import Conduit
import Vision.Image

colSums, rowSums :: Matrix R -> Vector R
rowSums m = m #> konst 1 (cols m)
colSums m = konst 1 (rows m) <# m

normalizeRows :: Matrix R -> Matrix R
normalizeRows m = m / asColumn (rowSums m)

-- | Gives the average of the sum of the rows of
--   some matrix. This is mostly used to take the
--   average output of a matrix where there is only
--   one non-zero value in every row
avgRowSum :: Matrix R -> Double
avgRowSum m = sumElements m / fromIntegral (rows m)

merge :: [a] -> [a] -> [a]
merge [] ys     = ys
merge (x:xs) ys = x:merge ys xs

scaleWord8 :: Double -> Word8 -> Word8
scaleWord8 c = min 255 . max 0 . round . min 255 . max 0 . (*c) . fromIntegral

readDigit :: Num a => Char -> Maybe a
readDigit '1' = Just 1
readDigit '2' = Just 2
readDigit '3' = Just 3
readDigit '4' = Just 4
readDigit '5' = Just 5
readDigit '6' = Just 6
readDigit '7' = Just 7
readDigit '8' = Just 8
readDigit '9' = Just 9
readDigit '0' = Just 0
readDigit _   = Nothing

type IOSrc a       = Source (ResourceT IO) a
type IOConduit a b = Conduit a (ResourceT IO) b
type IOSink a      = Sink a (ResourceT IO) ()

-- | The atomic output of a dataset. It consists of an image and all
--   associated labels. These are generally collected in batches for
--   training.
type LabeledImage = (RGBDelayed, [Maybe Int])

instance (Element x, Serialize x) => Serialize (Matrix x) where
  put = put . toLists
  get = fmap fromLists get

instance (Element x, Serialize x) => Serialize (Vector x) where
  put = put . toList
  get = fmap fromList get

