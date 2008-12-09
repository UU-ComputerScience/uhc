module Main where

choose :: a -> a -> a
choose a b = a

f1 :: ((forall a . Int -> a -> [a]) -> (forall a . Int -> a -> [a])) -> Int
f1 _ = 5
f2 :: (forall a . (Int -> a -> [a]) -> Int -> a -> [a]) -> Int
f2 _ = 6

v1 = (f1 (choose replicate),f2 (choose replicate))
-- ch :: forall a . (Int -> a -> [a]) -> Int -> a -> [a]
ch = choose replicate
v2 = (f1 ch,f2 ch)

main = return ()
