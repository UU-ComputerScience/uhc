module Main where

ids :: [forall a . a -> a]
ids = []

ht1  = head ids : ids
ht2  = (head ids :: forall a . a -> a) : ids
ht3  = head ~ids : ids

ht4 = head ids
ht5 = tail ids
ht6 = id   ids

ht7 = head ~ids
ht8 = tail ~ids
ht9 = id   ~ids

main = return ()
