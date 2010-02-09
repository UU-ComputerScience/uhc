{- ----------------------------------------------------------------------------------------
   what    : name introduction
   expected: ok, for function binding
---------------------------------------------------------------------------------------- -}

module Main where

-- ok
fok x = '4'
fok x = '5'

main :: IO ()
main = putStr "Dummy"
