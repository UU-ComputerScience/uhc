{- ----------------------------------------------------------------------------------------
   what    : RealFrac functions for Double
   expected: all ok
---------------------------------------------------------------------------------------- -}

module RealFracDouble1 where

main
  = do let pf1 :: (Int,Double)
           pf1@(p1,f1) = properFraction (1.55::Double)
       putStrLn (show p1)
       putStrLn (show f1)
       putStrLn (show (truncate (1.55::Double) :: Int))
       putStrLn (show (round (1.55::Double) :: Int))
       putStrLn (show (round (1.45::Double) :: Int))
       putStrLn (show (ceiling (1.45::Double) :: Int))
       putStrLn (show (floor (1.45::Double) :: Int))
