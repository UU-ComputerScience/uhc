{- ----------------------------------------------------------------------------------------
   what    : polarity, its use by subsumption
   expected: ok
---------------------------------------------------------------------------------------- -}

module Main where

l1 :: [forall a . a -> a]
l1 = [id]

l2 :: [Int -> Int]
l2 = l1

main = putStrLn ""
