{- ----------------------------------------------------------------------------------------
   what    : RealFrac functions for Rational
   expected: all ok
---------------------------------------------------------------------------------------- -}

module RealFracRational1 where

main
  = do let pf1 :: (Int,Rational)
           pf1@(p1,f1) = properFraction (fromDouble (1.55::Double))
       putStrLn (show p1)
       putStrLn (show f1)
       putStrLn (show (truncate (fromDouble (1.55::Double) :: Rational) :: Int))
       putStrLn (show (round (fromDouble (1.55::Double) :: Rational) :: Int))
       putStrLn (show (round (fromDouble (1.45::Double) :: Rational) :: Int))
       putStrLn (show (ceiling (fromDouble (1.45::Double) :: Rational) :: Int))
       putStrLn (show (floor (fromDouble (1.45::Double) :: Rational) :: Int))


