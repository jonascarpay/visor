{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Buffer where

import Conduit
import Types
import Data.Maybe
import Data.Singletons.TypeLits
import Data.Proxy

newtype Buffer (size :: Nat) game = Buffer [game]

instance Monoid (Buffer s a) where
  mempty = Buffer []
  Buffer a `mappend` Buffer b = Buffer (a `mappend` b)

bufHead (Buffer [])    = Nothing
bufHead (Buffer (h:_)) = Just h

denoiseC :: forall a n. (KnownNat n, Transitions a) => RTConduit a (Buffer n a)
denoiseC = myFold mempty .| mapC Buffer
  where
    bufsize = fromIntegral$ natVal (Proxy :: Proxy n)

    mend h@(log, buf)
      | null buf                  = h
      | buflen >= length log      = (buf,[])
      | buflen >= bufsize         = (buf ++ log, [])
      | head logtail ->? last buf = (buf ++ logtail, [])
      | otherwise                 = h
      where logtail = drop buflen log
            buflen = length buf

    myFold s = do mx <- await
                  case mx of
                    Nothing -> return ()
                    Just x  ->
                      do let !s' = foldf s x
                         yield (fst s')
                         myFold s'

    foldf ([],     _     ) !st            =      ([st],   [])
    foldf (l:(!t), _     ) !st | l ->? st =      (st:l:t, [])
    foldf (log,    b:(!t)) !st | b ->? st = mend (log,    st:b:t)
    foldf (!log,   _     ) !st            = mend (log,    [st]  )

watchC :: (KnownNat s, Transitions a, GameState a) => RTConduit (LabelVec a) (Buffer s a)
watchC = mapC delabel .| denoiseC .| printBufHeadC

printBufHeadC :: GameState a => RTConduit (Buffer s a) (Buffer s a)
printBufHeadC = awaitForever $ \buf ->
  do liftIO . putStrLn . fromMaybe "" . fmap pretty . bufHead $ buf
     yield buf

