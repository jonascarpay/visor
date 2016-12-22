module Util where

import Conduit

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

