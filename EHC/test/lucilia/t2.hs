module Main where

ids :: [forall a . a -> a]
ids = []

h1  = (\x -> x) : ids
h2  :: [forall a . a -> a]
h2  = (\x -> x) : ids

h3 = id ids

main = return ()
