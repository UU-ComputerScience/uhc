{- ----------------------------------------------------------------------------------------
   what    : Bounded class, for Int
   expected: ok
---------------------------------------------------------------------------------------- -}

module Main where

main :: IO ()
main
  = do putStrLn (show (minBound :: Int))
       putStrLn (show (maxBound :: Int))
