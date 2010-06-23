{- ----------------------------------------------------------------------------------------
   what    : name introduction
   expected: error, report of duplicates
---------------------------------------------------------------------------------------- -}

module Main where

-- class
class A a where
  a1 :: a -> a
class A a where
  a2 :: a -> a

-- data constructor
data X1 = X
data X2 = X

-- type via data
data Y1 = Y1
data Y1 = Y2

-- kind signature
Y1 :: *
Y1 :: *

main :: IO ()
main = putStr "Dummy"
