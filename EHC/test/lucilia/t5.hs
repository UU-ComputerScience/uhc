module Main where

choose :: a -> a -> a
choose a b = a

f1 :: ((forall a . a -> a) -> (forall a . a -> a)) -> Int
f1 _ = 5
f2 :: (forall a . (a -> a) -> a -> a) -> Int
f2 _ = 6

v1 = (f1 (choose id),f2 (choose id))
ch = choose id
v2 = (f1 ch,f2 ch)

main = return ()
