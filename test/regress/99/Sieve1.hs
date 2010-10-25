{- ----------------------------------------------------------------------------------------
   what    : just a program used for basic 'is it up and running'
   expected: ok, takes some time depending on target
---------------------------------------------------------------------------------------- -}

module Sieve1 where

notMultiple x y = not ((y `div` x) * x == y)
sieve (h:t) = h : sieve (filter (notMultiple h) t)

main :: IO ()
main
  = putStrLn (show (last (take (100::Int) (sieve [(2::Int)..]))))
