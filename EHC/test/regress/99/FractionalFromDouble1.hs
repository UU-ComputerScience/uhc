{- ----------------------------------------------------------------------------------------
   what    : Fractional.fromDouble for various types
   expected: all ok
---------------------------------------------------------------------------------------- -}

module FractionalFromDouble1 where

main
  = do let r1 = fromDouble (10.5::Double) :: Float
       putStrLn (show r1)
       let r2 = fromDouble (10.5::Double) :: Double
       putStrLn (show r2)
       let r3 = fromDouble (10.5::Double) :: Rational
       putStrLn (show r3)
       putStrLn (show (numerator r3))
       putStrLn (show (denominator r3))
       let r4 = fromDouble (10.5::Double) :: Ratio Int
       putStrLn (show r4)
       putStrLn (show (numerator r4))
       putStrLn (show (denominator r4))

