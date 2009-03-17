{- ----------------------------------------------------------------------------------------
   what    : Bounded class, for Char
   expected: ok
---------------------------------------------------------------------------------------- -}

module BoundedChar1 where

main :: IO ()
main
  = do putStrLn (show (minBound :: Char))
       putStrLn (show (maxBound :: Char))
