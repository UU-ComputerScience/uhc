module Main where

choose  ::  a -> a -> a
choose x y = x

v2      =   choose id

v3      =   (choose :: (forall a . a -> a) -> (forall b . b -> b) -> (forall c . c -> c)) id

v4      ::  (forall a . a -> a) -> (forall b . b -> b)
v4      =   choose id
v5      =   choose ~id

f :: (forall a . a->a) -> (Int,Char)
f i = (i (5::Int), i 'x')

f1 :: (a->a) -> Int
f1 i = i (5::Int)

f2   = f1
f3 i = f1 i

v2b i = v2 i
v4b i = v4 i

main = return ()
