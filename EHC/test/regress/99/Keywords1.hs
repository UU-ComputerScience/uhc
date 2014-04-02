{- ----------------------------------------------------------------------------------------
   what    : test for previous bug, keyword exists was unusable as variable ident
   expected: ok
---------------------------------------------------------------------------------------- -}

module Main where

main = let exists = undefined
        in return ()
