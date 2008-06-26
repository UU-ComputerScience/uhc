{- ----------------------------------------------------------------------------------------
   what    : type synonym
   expected: error about limit of tysyn expansion
---------------------------------------------------------------------------------------- -}

module Main where

type X a = X a

x :: X Int
x = 3::Int

main = return ()