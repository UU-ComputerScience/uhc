module Main where

f :: a -> [a] -> Int
f _ _ = 5

foo :: [Int -> forall b. b -> b]
foo = []

bog = f (\x y -> y) foo

main = return ()
