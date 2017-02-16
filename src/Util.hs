{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DataKinds #-}

module Util where

import Conduit

type RTSource  a   = Source    (ResourceT IO) a
type RTConduit a b = Conduit a (ResourceT IO) b
type RTSink    a   = Sink    a (ResourceT IO) ()

divs :: Integral a => a -> a -> Bool
a `divs` b = b `mod` a == 0

count :: Eq a => a -> [a] -> Int
count needle = length . filter (==needle)
