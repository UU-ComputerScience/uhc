{- ----------------------------------------------------------------------------------------
   what    : Bounded class, for Char
   expected: ok
---------------------------------------------------------------------------------------- -}

module BoundedChar1 where
import System.IO

main :: IO ()
main
  = do putStrLn (show (minBound :: Char))
       putStrLn (show (maxBound :: Char))
