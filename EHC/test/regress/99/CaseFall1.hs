{- ----------------------------------------------------------------------------------------
   what    : fallthrough of case alternatives for single var + guard
   expected: ok, 16
---------------------------------------------------------------------------------------- -}

module CaseFall1 where

fib :: Int -> Int
fib n | n == 0 = n
fib n | n == 1 = n
fib n = fib (n-1) + fib (n-1)

main = putStrLn (show (fib 5))
