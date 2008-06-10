module Main where

f :: ((forall a. a) -> Bool) -> Bool
f _ = True

g:: (forall a.a->a) -> (forall a.a)
g _ = undefined

h = f g

main = return ()
