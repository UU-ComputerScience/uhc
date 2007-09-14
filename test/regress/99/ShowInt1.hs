{- ----------------------------------------------------------------------------------------
   what    : show of Int values
   expected: ok
---------------------------------------------------------------------------------------- -}

module Main where

main
  = do putStrLn (show (0::Int))
       putStrLn (show (1::Int))
       putStrLn (show ((-1)::Int))
       putStrLn (show ( 123456789::Int))
       putStrLn (show (-123456789::Int))

