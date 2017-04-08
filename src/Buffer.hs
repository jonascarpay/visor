{-# LANGUAGE ScopedTypeVariables #-}

module Buffer where

import Conduit
import Types
import Safe

-- | If x1 and x2 could be values for some x in two
--   subsequent screen polls, then x1 ->? x2.
--   This is used for discarding bad classifications in noisy streams.
class Eq a => Transitions a where
  (->?) :: a -> a -> Bool

type Buffer a = ([a],[a])
bufferedFilter :: forall a. Transitions a => Int -> RTConduit a a
bufferedFilter bufsize = go mempty
  where
    go s = do mx <- await
              case mx of
                Nothing -> return ()
                Just x  ->
                  do let (mx, s') = buffer x s
                     yieldMany mx
                     go s'

    buffer :: a -> Buffer a-> (Maybe a, Buffer a)
    buffer st ([], _)
      = (Nothing, ([st],[]))
    buffer st (log@(h:_), buf)
      | h ->? st = (log `atMay` (bufsize-1), (log', []))
      | null buf || not (head buf ->? st) = (Nothing, mend log [st])
      | fullBuf buf = (Just$ last buf, (buf',[]))
      | otherwise   = (Nothing, mend log buf')
      where
        fullBuf buf = length buf == bufsize
        buf' = take bufsize$ st:buf
        log' = take bufsize$ st:log

    mend log buf | length log <= length buf = (buf, [])
    mend log [] = (log, [])
    mend log buf | (log !! length buf) ->? last buf = (buf ++ drop (length buf) log, [])
    mend log buf = (log, buf)
