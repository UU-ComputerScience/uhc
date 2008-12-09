{- ----------------------------------------------------------------------------------------
   what    : name introduction
   expected: error, report of duplicates
---------------------------------------------------------------------------------------- -}

module Main where

-- pattern binding in list comprehension
l = [ a | (a,a) <- [], let (b,b) = a ]

-- pattern binding in monad comprehension
m = do (c,c) <- return ()
       let (d,d) = c
       return ()

main :: IO ()
main = putStr "Dummy"

