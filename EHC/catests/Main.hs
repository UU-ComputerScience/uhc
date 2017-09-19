module Main where

import Test hiding (fo, t6)

main :: IO ()
main = print t6


fo :: (a -> b -> a) -> a -> [b] -> a
fo = \f z0 xs0 -> let
  lgo = \z ys -> case ys of
    [] -> z
    (x:xs) -> let z' = f z x in lgo z' xs
  in lgo z0 xs0

t6 :: Int
t6 = fo (+) 0 [1 .. 10000000]