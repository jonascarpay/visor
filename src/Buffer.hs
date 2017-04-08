{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Buffer where

import Conduit
import Types
import Data.Singletons.TypeLits
import Data.Proxy
import Safe

newtype Buffer (size :: Nat) game = Buffer [game]

instance Monoid (Buffer s a) where
  mempty = Buffer []
  Buffer a `mappend` Buffer b = Buffer (a `mappend` b)

bufHeadRaw :: Buffer s a -> Maybe a
bufHeadRaw (Buffer [])    = Nothing
bufHeadRaw (Buffer (h:_)) = Just h

bufHead :: forall s a. KnownNat s => Buffer s a -> Maybe a
bufHead (Buffer buf) = buf `atMay` bufsize
  where
    bufsize = fromIntegral$ natVal (Proxy :: Proxy s)

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

type Watch s a = RTConduit (LabelVec a) (Buffer s a)
watchC :: (KnownNat s, Transitions a, GameState a) => Watch s a
watchC = mapC delabel .| denoiseC .| printBufHeadC

printBufHeadC :: GameState a => RTConduit (Buffer s a) (Buffer s a)
printBufHeadC = awaitForever $ \buf ->
  do liftIO . putStrLn . maybe "" pretty . bufHeadRaw $ buf
     yield buf

