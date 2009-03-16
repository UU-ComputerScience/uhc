{- ----------------------------------------------------------------------------------------
   what    : RealFloat basic functions for Double
   expected: all ok
---------------------------------------------------------------------------------------- -}

module RealFloatDouble1 where

main
  = do putStrLn (show (isNaN (1.0::Double)))
       putStrLn (show (isDenormalized (1.0::Double)))
       putStrLn (show (isInfinite (1.0::Double)))
       putStrLn (show (isInfinite ((1.0::Double) / 0.0)))
       putStrLn (show (isIEEE (1.0::Double)))
       putStrLn (show (isNegativeZero (0.0::Double)))
       putStrLn (show (isNegativeZero (-0.0::Double)))
       putStrLn (show (floatRadix (1.0::Double)))
       putStrLn (show (floatDigits (1.0::Double)))
       putStrLn (show (fst $ floatRange (1.0::Double)))
       putStrLn (show (snd $ floatRange (1.0::Double)))
       let df1 = decodeFloat (100.0::Double)
           df2 = decodeFloat (-100.0::Double)
           df3 = decodeFloat (0.0::Double)
       putStrLn (show (fst df1))
       putStrLn (show (snd df1))
       putStrLn (show (fst df2))
       putStrLn (show (snd df2))
       putStrLn (show (fst df3))
       putStrLn (show (snd df3))
       putStrLn (show (uncurry encodeFloat df1 :: Double))
       putStrLn (show (uncurry encodeFloat df2 :: Double))
       putStrLn (show (uncurry encodeFloat df3 :: Double))
