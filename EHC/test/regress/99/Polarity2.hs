{- ----------------------------------------------------------------------------------------
   what    : polarity, its use by subsumption
   expected: type error for l3
---------------------------------------------------------------------------------------- -}

module Main where

l1 :: [forall a . a -> a]
l1 = [id]

l2 :: [Int -> Int]
l2 = l1

l3 :: [forall a . a -> a]
l3 = l2

main = putStrLn ""
