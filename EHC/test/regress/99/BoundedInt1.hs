{- ----------------------------------------------------------------------------------------
   what    : Bounded class, for Int
   expected: ok
   platform: word size dependent
---------------------------------------------------------------------------------------- -}

module BoundedInt1 where

main :: IO ()
main
  = do putStrLn (show (minBound :: Int))
       putStrLn (show (maxBound :: Int))
