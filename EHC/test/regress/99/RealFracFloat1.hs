{- ----------------------------------------------------------------------------------------
   what    : RealFrac functions for Float
   expected: all ok
---------------------------------------------------------------------------------------- -}

module RealFracFloat1 where

main
  = do let pf1 :: (Int,Float)
           pf1@(p1,f1) = properFraction (1.55::Float)
       putStrLn (show p1)
       putStrLn (show f1)
       putStrLn (show (truncate (1.55::Float) :: Int))
       putStrLn (show (round (1.55::Float) :: Int))
       putStrLn (show (round (1.45::Float) :: Int))
       putStrLn (show (ceiling (1.45::Float) :: Int))
       putStrLn (show (floor (1.45::Float) :: Int))
