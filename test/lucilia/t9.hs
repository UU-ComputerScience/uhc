module Main where

f1 :: forall a . a -> [a] -> [a]
ids :: [forall a. a->a]
h1 = f1 (\x->x) ids 
h2 = f1 (\x->x) ~ids 

main = return ()
