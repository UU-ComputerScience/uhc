{- ----------------------------------------------------------------------------------------
   what    : Bounded class, for Char
   expected: ok
---------------------------------------------------------------------------------------- -}

module Main where

main :: IO ()
main
  = do putStrLn (show (minBound :: Char))
       putStrLn (show (maxBound :: Char))
