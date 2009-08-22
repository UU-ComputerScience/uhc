{- ----------------------------------------------------------------------------------------
   what    : name introduction
   expected: error, report of duplicates
---------------------------------------------------------------------------------------- -}

module Main where

-- pattern binding in function binding
f (a,a) = a

-- pattern binding in lambda
g = \(b,b) -> b

-- pattern binding in case alternative
v = case ('2','3') of
      (c,c) -> c


main :: IO ()
main = putStr "Dummy"
