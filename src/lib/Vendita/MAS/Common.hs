module Vendita.MAS.Common (mapPair, toPair) where

mapPair :: (a -> c, b -> d) -> (a, b) -> (c, d)
mapPair (fa, fb) (a, b) = (fa a, fb b)

toPair :: (a -> b, a -> c) -> a -> (b, c)
toPair (fa, fb) a = (fa a, fb a)