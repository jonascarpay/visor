{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module ImageProcessing where

import Data.Array.Accelerate as A
import Data.Word
import Prelude hiding (zipWith3, map, fromIntegral, round, unzip3, replicate)

type Image a = Acc(Array DIM2 a)
type Grey = Float
type RGB = (Float, Float, Float)

packGrey :: Acc (Array DIM3 Word8) -> Image Grey
packGrey a = zipWith3 plus r g b
  where
    plus x y z = x + y + z
    channel i = map fromIntegral $ slice a (lift $ Any :. (i :: Int))
    r = map (*0.21) $ channel 0
    g = map (*0.72) $ channel 1
    b = map (*0.07) $ channel 2

unpackGrey :: Image Grey -> Acc (Array DIM3 Word8)
unpackGrey = replicate (lift $ Z:.All:.All:.(3::Int)) . map round
