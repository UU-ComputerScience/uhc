{- ----------------------------------------------------------------------------------------
   what    : name introduction
   expected: error, report of duplicates
---------------------------------------------------------------------------------------- -}

module Main where

-- type signature
d :: Int
d :: Char

f :: Int -> Char
f :: Int -> Char

-- pattern binding
d = 'c'
d = 'f'

-- fixity
infixr +, +

-- data field
data FF = FF {ff :: Int, ff :: Int}

main :: IO ()
main = putStr "Dummy"
