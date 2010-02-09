{- ----------------------------------------------------------------------------------------
   what    : Real functions for Double
   expected: all ok
---------------------------------------------------------------------------------------- -}

module Main where

main
  = do let r = toRational (10::Double)
       putStrLn (show (numerator r))
       putStrLn (show (denominator r))
       let r = toRational (0.8::Double)
       putStrLn (show (numerator r))
       putStrLn (show (denominator r))
