module Main where

ids :: [forall a . a -> a]
ids = []

h1  = head ids : ids
h2  = (head ids :: forall a . a -> a) : ids
-- h3  = (~(head ids)) : ids

h4 = tail ids
h5 = id   ids

main = return ()
