{- ----------------------------------------------------------------------------------------
   what    : RealFloat derived functions for Double
   expected: all ok
---------------------------------------------------------------------------------------- -}

module Main where

main
  = do putStrLn (show (exponent (100.0::Double)))
       putStrLn (show (significand (100.0::Double)))
       putStrLn (show (scaleFloat 2 (100.0::Double)))
       putStrLn (show (atan (1.0::Double)))
       putStrLn (show (atan2 (1.0::Double) 1))
