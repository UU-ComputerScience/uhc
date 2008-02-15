{- ----------------------------------------------------------------------------------------
   what    : Real functions for Float
   expected: all ok
---------------------------------------------------------------------------------------- -}

module Main where

main
  = do let r = toRational (10::Float)
       putStrLn (show (numerator r))
       putStrLn (show (denominator r))
       let r = toRational (0.8::Float)
       putStrLn (show (numerator r))
       putStrLn (show (denominator r))
