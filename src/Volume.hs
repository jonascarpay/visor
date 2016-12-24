{-# OPTIONS_GHC -Odph -rtsopts -threaded -fno-liberate-case
                -funfolding-use-threshold1000 -funfolding-keeness-factor1000
                -fllvm -optlo-O3 #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveGeneric #-}

module Volume where

import Label
import Data.Word
import Data.Array.Repa as R hiding ((++))
import qualified Data.Vector.Unboxed as DV
import Data.Array.Repa.Algorithms.Randomish
import GHC.Generics (Generic)
import Data.Serialize

type Weights = Array U DIM4 Double
type Volume  = Array U DIM3 Double
type Matrix  = Array U DIM2 Double
type Vector  = Array U DIM1 Double
type Bias    = Volume
type DWeights = Array D DIM4 Double
type DVolume  = Array D DIM3 Double
type DMatrix  = Array D DIM2 Double
type DVector  = Array D DIM1 Double

-- | A network layer that has volumes as both its input and output.
data Layer3
  = Conv -- ^ A layer that applies a convolution of its weights (or,
         --   more accurately, a correlation) to its input volume.
         Weights -- ^ The weights used for the convolution. The first
                 --   dimension, i, is used to distinguish different weights.
                 --   Its second dimension, d, is equal to the input volume's
                 --   first dimension d, and therefore disappears jrom the output
                 --   in order to produce a volume again.
         Bias    -- ^ A bias volume that is added to the output volume.
  | ReLU -- ^ A rectified linear unit, or ReLU. NN jargon for (\x -> max x 0).
         --   A ReLU is cheap but has all the properties that you want from an
         --   activation function and hence is the most commonly used.
  | Pool -- ^ A max-pooling layer. The pool size has been hard-coded to 2, at least
         --   for now. A pooling layer subsamples the input to a quarter the size,
         --   passing through the maximum element in each 2x2 subregion.
         deriving (Eq, Generic)
instance Serialize Layer3

instance Show Layer3 where
  show Pool       = "Pool"
  show ReLU       = "ReLU"
  show (Conv w b) = "Conv dimW: " ++ dimW ++ " dimB: " ++ dimB
    where
      dimW = show . extent $ w
      dimB = show . extent $ b

-- | Apply a volume to a Layer3
forward3 :: Monad m -- ^ Repa requires some monad in order to guarantee
                    --   that parallel evaluations are executed sequentially.
         => Volume
         -> Layer3
         -> m Volume
forward3 x (Conv w b) = w `corr` x >>= computeP . (+^ b)

forward3 x ReLU       = computeP $ R.map (max 0) x
forward3 x Pool       = pool x

softMax :: Monad m => Vector -> [Int] -> m Vector
softMax x cs = do maxElem <- foldAllP max (-1/0) x
                  (exps   :: Vector) <- computeP $ R.map (exp . subtract maxElem) x
                  let splits = splitCs cs $ toUnboxed exps
                      crossEntropy = DV.concat $ fmap normalize splits
                  return $ fromUnboxed (extent x) crossEntropy

splitCs :: [Int] -> DV.Vector Double -> [DV.Vector Double]
splitCs [] _ = []
splitCs (c:cs) rem = let (h,t) = DV.splitAt c rem in h:splitCs cs t

normalize :: DV.Vector Double -> DV.Vector Double
normalize vec = DV.map (/s) vec
  where s = DV.sum vec

softMaxBackward :: Monad m => Vector -> [Int] -> [Label] -> m Vector
softMaxBackward vec cs ls = computeP$ R.traverse vec id lkFn
  where
    offsets = scanl (+) 0 cs
    ixs = Prelude.zipWith (+) offsets (fromLabel <$> ls)
    lkFn lkUp (Z:.i) = if i `elem` ixs then lkUp (ix1 i) -1 else lkUp (ix1 i)

getMaxima :: Vector -> [Int] -> [Int]
getMaxima = undefined

-- | Propagate an error gradient backwards through a Layer3. Some arguments
--   are calculated during the forward pass. We could recalculate them
--   during the backwards pass, but for the sake of both efficiency
--   and clarity I chose to reuse them from the forward pass. The recursive
--   definition of the training function makes this work out quite nicely.
backward3 :: Monad m -- ^ Required by repa for parallel computations
          => Layer3 -- ^ Layer to backprop through
          -> Volume -- ^ Input for this layer during the forward pass
          -> Volume -- ^ Output for this layer during the forward pass
          -> Volume -- ^ Error gradient on the output of this layer
          -> Double -- ^ Regularization loss factor. Not yet implemented.
          -> Double -- ^ Step size/learning rate
          -> m (Layer3, Volume) -- ^ Updated Layer3 with new weights, and error gradient on this layer's input.
backward3 (Conv w b) x _ dy _ α =
  do dx <- w `fullConv` dy
     dw <- dy `corrVolumes` x
     db <- computeP$ b -^ R.map (*α) dy
     w' <- computeP$ w -^ R.map (*α) dw
     return (Conv w' db, dx)

backward3 Pool x y dy _ _ =
  do dx <- poolBackprop x y dy
     return (Pool, dx)

backward3 ReLU _ y dy _ _ =
  do dx <- computeP $ R.zipWith (\x t -> if t > 0 then x else 0) dy y
     return (ReLU, dx)

-- TODO: backprop van pooling moet extent-invariant worden
-- | Max-pooling function for volumes
pool :: Monad m => Volume -> m Volume
pool v = computeP $ R.traverse v shFn maxReg
  where
    n = 2
    shFn (Z:.d:.h:.w) = Z:. d :. h `div` n :. w `div` n
    maxReg lkUp (b:.y:.x) = maximum [ lkUp (b:.y*n + dy:.x*n + dx) | dy <- [0..n-1], dx <- [0 .. n-1]]

-- | Backprop of the max-pooling function. We upsample the error volume,
--   propagating the error to the position of the max element in every subregion,
--   setting the others to 0.
poolBackprop :: Monad m
             => Volume -- ^ Input during forward pass, used to determine max-element
             -> Volume -- ^ Output during forward pass, used to determine max-element
             -> Volume -- ^ Error gradient on the output
             -> m Volume -- ^ Error gradient on the input
poolBackprop input output errorGradient = computeP $ traverse3 input output errorGradient shFn outFn
  where
    n = 2
    shFn sh _ _ = sh
    {-# INLINE outFn #-}
    outFn in_ out_ err_ p@(Z:.z:.y:.x) = if out_ p' == in_ p then err_ p' else 0
      where p' = Z:. z :. y `div` n :. x `div` n

-- | Rotates the two topmost dimensions of an array by 180 degrees.
{-# INLINE rotate #-}
rotate :: (Source r e, Shape tail) => Array r ((tail :. Int) :. Int) e -> Array D ((tail :. Int) :. Int) e
rotate arr = backpermute sh invert arr
  where
    sh@(_:.h:.w) = extent arr
    invert (b:.y:.x) = b:.h-y-1:.w-x-1

-- | Rotates the two topmost dimensions of an array by 180 degrees.
{-# INLINE rotateW #-}
rotateW :: Weights -> DWeights
rotateW arr = backpermute (Z:.d:.n:.h:.w) invert arr
  where
    Z:.n:.d:.h:.w = extent arr
    invert (Z:.i:.z:.y:.x) = Z:.z:.i:.h-y-1:.w-x-1

-- | Valid convolution of a stencil/kernel over some image, with the
--   kernel rotated 180 degrees. The valid part means that no zero
--   padding is applied. It is called corr to reflect that the
--   correct term for this operation would be cross-correlation.
--   Cross-correlation and convolution are used interchangeably in
--   most literature which can make things very confusing.
--   Note that the kernel is 4-dimensional, while the image is
--   three-dimensional. The stencils first dimension translates
--   to the output volume's depth.
--   A kernel of size (Z:. n_k :. d_k :. h_k :. w_k) convolved over
--   an image of size (Z:. d_i :. h_i :. w_i) results in
--   a output of size (Z:. n_k :. h_i - h_k +1 :. w_i - w_k + 1)
corr :: Monad m -- ^ Host monad for repa
     => Weights -- ^ Convolution kernel
     -> Volume  -- ^ Image to iterate over
     -> m Volume
corr krns img = if kd /= id
                   then error $ "kernel / image depth mismatch, k:" ++ show (extent krns) ++ " i:" ++ show (extent img)
                   else computeP $ fromFunction sh' convF
  where
    Z:.kn:.kd:.kh:.kw = extent krns
    Z:.    id:.ih:.iw = extent img
    sh' = Z:.kn:.ih-kh+1:.iw-kw+1

    {-# INLINE convF #-}
    convF :: DIM3 -> Double
    convF (Z:.od:.oh:.ow) = sumAllS $ krn *^ img'
      where
        krn  = slice krns (Z:.od:.All:.All:.All)
        img' = extract (Z:.0:.oh:.ow) (Z:.id:.kh:.kw) img

corrVolumes :: Monad m => Volume -> Volume -> m Weights
corrVolumes krns imgs = computeP $ fromFunction sh' convF
  where
    Z:.kd:.kh:.kw = extent krns
    Z:.id:.ih:.iw = extent imgs
    sh' :: DIM4
    sh' = Z :. kd :. id :. ih-kh+1 :. iw-kw+1

    {-# INLINE convF #-}
    convF :: DIM4 -> Double
    convF (Z:.n:.z:.y:.x) = sumAllS $ krn *^ img
      where
        krn = extract (ix3 n 0 0) (ix3 1 kh kw) krns
        img = extract (ix3 z y x) (ix3 1 kh kw) imgs

conv :: Monad m => Volume -> Volume -> m Weights
conv krn img = do krn' <- computeP $ rotate krn
                  corrVolumes krn' img

fullConv :: Monad m => Weights -> Volume -> m Volume
fullConv krn img = do krn' <- computeP $ rotateW krn
                      img' <- computeP $ zeropad (kh-1) img
                      krn' `corr` img'
  where
    _:.kh:._ = extent krn

{-# INLINE zeropad #-}
zeropad :: (Source r Double, Shape tail) => Int -> Array r ((tail :. Int) :. Int) Double -> Array D ((tail :. Int) :. Int) Double
zeropad n a = R.traverse a shFn padFn
  where
    _:.h:.w = extent a

    {-# INLINE shFn #-}
    shFn (b:.h:.w) = b:.h+2*n:.w+2*n
    {-# INLINE padFn #-}
    padFn lkFn (b:.y:.x)
      | y < n || y >= h + n || x < n || x >= w + n = 0
      | otherwise = lkFn (b:.y-n:.x-n)

{-# INLINE vmmult #-}
vmmult :: (Source r Double, Source r2 Double) => Array r2 DIM1 Double -> Array r DIM2 Double -> Array D DIM1 Double
vmmult v m = fromFunction sh' ixFn
  where
    Z:._:.c = extent m
    sh' = Z:.c
    ixFn (Z:.i :: DIM1) = sumAllS $ slice m (Any:.i) *^ v

-- | Vector-vector multiplication. The output is a matrix in which every
--   (x,y) position corresponds to the xth element of the first vector
--   and the yth element of the second. Hence, the resulting matrix is
--   as wide as the first vector's length and as tall as the second's.
{-# INLINE vvmult #-}
vvmult :: (Source r2 Double, Source r1 Double) => Array r1 DIM1 Double -> Array r2 DIM1 Double -> Array D DIM2 Double
vvmult vw vh = traverse2 vw vh shFn vFn
  where
    shFn (Z:.w) (Z:.h) = Z:.h:.w
    vFn v1 v2 (Z:.y:.x) = v1 (ix1 x) * v2 (ix1 y)

-- | Data loss of a network output, given some classification.
--   This value is somewhere between 0 (p_correct == 1) and
--   infinity (p_correct == 0).
dataLoss :: Vector -> Label -> Double
dataLoss p (fromLabel -> i) = negate . log $ linearIndex p i

-- | The label of some input to a network, as determined by
--   the output of the network for that input.
--   If the output layer of the network is a SoftMax function,
--   the class scores produced by that network can be interpreted
--   as probability/certainty scores. This function returns the
--   class the network assigns the highest probability to and
--   converts it into a label.
maxIndex :: Vector -> Label
maxIndex = toLabel . DV.maxIndex . toUnboxed

maxElem :: (Source r Double, Shape sh, Monad m) => Array r sh Double -> m Double
maxElem = foldAllP max (-1/0)

minElem :: (Source r Double, Shape sh, Monad m) => Array r sh Double -> m Double
minElem = foldAllP min (1/0)

subtractOneAt :: Monad m => Int -> Vector -> m Vector
subtractOneAt i arr = computeP $ R.traverse arr id ixFn
  where ixFn src (Z:.w) = if w == i then src (ix1 w) - 1 else src (ix1 w)

-- TODO: Non-urgent; Let forward1 accept delayed representations so the
-- flattened array does not need to be rebuilt in memory
flatten :: (Monad m, Source r1 Double, Shape sh1) => Array r1 sh1 Double -> m Vector
flatten arr = computeP $ reshape (Z:.s) arr
  where s = product . listOfShape . extent $ arr

randomConvLayer :: Int -- ^ Kernel size
                -> Int -- ^ Kernel/input depth
                -> Int -- ^ Kernel count
                -> Int -- ^ Input width
                -> Int -- ^ Input height
                -> Int -- ^ RNG Seed
                -> Layer3
randomConvLayer ks kd kn iw ih seed = Conv w b
  where
    w = randomishDoubleArray (Z:.kn:.kd:.ks:.ks)       1e-2 (-1e-2) seed
    b = randomishDoubleArray (Z:.kn:.ih-ks+1:.iw-ks+1) 1e-2 (-1e-2) (seed+1)

toCifarVolume :: [Word8] -> Volume
toCifarVolume = fromListUnboxed (Z:.3:.32:.32) . fmap ((/255) . fromIntegral)

lerp :: (Source r Double, Shape sh, Monad m) => Array r sh Double -> Double -> Double -> m (Array U sh Double)
lerp arr lo hi = do minE <- minElem arr
                    maxE <- maxElem arr
                    computeP $ R.map (\x -> lo + (x-minE) * (hi-lo) / (maxE-minE)) arr

splitW :: Monad m => Weights -> m [Matrix]
splitW arr = Prelude.traverse computeP slices
  where
    Z:.n:.d:._:._ = extent arr
    slices :: [DMatrix]
    slices = [ slice arr (Z:.ni:.di:.All:.All) | ni <- [0..n-1], di <- [0..d-1]]

getWeights :: [Layer3] -> [Weights]
getWeights [] = []
getWeights (Conv w _:ws) = w : getWeights ws
getWeights (_:ws) = getWeights ws

instance (DV.Unbox e, Serialize e) => Serialize (Array U DIM4 e) where
  put a = do put (listOfShape . extent $ a)
             put (toList a)
  get   = do sh    <- shapeOfList <$> get
             elems <- get
             return (fromListUnboxed sh elems)

instance (DV.Unbox e, Serialize e) => Serialize (Array U DIM3 e) where
  put a = do put (listOfShape . extent $ a)
             put (toList a)
  get   = do sh    <- shapeOfList <$> get
             elems <- get
             return (fromListUnboxed sh elems)

instance (DV.Unbox e, Serialize e) => Serialize (Array U DIM2 e) where
  put a = do put (listOfShape . extent $ a)
             put (toList a)
  get   = do sh    <- shapeOfList <$> get
             elems <- get
             return (fromListUnboxed sh elems)

instance (DV.Unbox e, Serialize e) => Serialize (Array U DIM1 e) where
  put a = do put (listOfShape . extent $ a)
             put (toList a)
  get   = do sh    <- shapeOfList <$> get
             elems <- get
             return (fromListUnboxed sh elems)
