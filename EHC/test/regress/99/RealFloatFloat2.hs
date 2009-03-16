{- ----------------------------------------------------------------------------------------
   what    : RealFloat derived functions for Float
   expected: all ok
---------------------------------------------------------------------------------------- -}

module RealFloatFloat2 where

main
  = do putStrLn (show (exponent (100.0::Float)))
       putStrLn (show (significand (100.0::Float)))
       putStrLn (show (scaleFloat 2 (100.0::Float)))
       putStrLn (show (atan (1.0::Float)))
       putStrLn (show (atan2 (1.0::Float) 1))
