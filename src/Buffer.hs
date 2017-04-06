module Buffer where

import Conduit
import Types
import Data.Maybe

newtype Buffer game = Buffer [game]

instance Monoid (Buffer a) where
  mempty = Buffer []
  Buffer a `mappend` Buffer b = Buffer (a `mappend` b)

bufHead (Buffer [])    = Nothing
bufHead (Buffer (h:_)) = Just h

denoiseC :: Transitions a => Int -> RTConduit a (Buffer a)
denoiseC bufsize = myFold mempty .| mapC Buffer
  where

    mend h@(log, buf)
      | null buf                  = h
      | buflen >= length log      = (buf,[])
      | buflen >= bufsize         = ((buf ++ log), [])
      | head logtail ->? last buf = ((buf ++ logtail), [])
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

printBufHeadC :: GameState a => RTConduit (Buffer a) (Buffer a)
printBufHeadC = awaitForever $ \buf ->
  do liftIO . print . fromMaybe "" . fmap pretty . bufHead $ buf
     yield buf

