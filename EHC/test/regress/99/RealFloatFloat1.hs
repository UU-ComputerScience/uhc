{- ----------------------------------------------------------------------------------------
   what    : RealFloat basic functions for Float
   expected: all ok
---------------------------------------------------------------------------------------- -}

module RealFloatFloat1 where

main
  = do putStrLn (show (isNaN (1.0::Float)))
       putStrLn (show (isDenormalized (1.0::Float)))
       putStrLn (show (isInfinite (1.0::Float)))
       putStrLn (show (isInfinite ((1.0::Float) / 0.0)))
       putStrLn (show (isIEEE (1.0::Float)))
       putStrLn (show (isNegativeZero (0.0::Float)))
       putStrLn (show (isNegativeZero (-0.0::Float)))
       putStrLn (show (floatRadix (1.0::Float)))
       putStrLn (show (floatDigits (1.0::Float)))
       putStrLn (show (fst $ floatRange (1.0::Float)))
       putStrLn (show (snd $ floatRange (1.0::Float)))
       let df1 = decodeFloat (100.0::Float)
           df2 = decodeFloat (-100.0::Float)
           df3 = decodeFloat (0.0::Float)
       putStrLn (show (fst df1))
       putStrLn (show (snd df1))
       putStrLn (show (fst df2))
       putStrLn (show (snd df2))
       putStrLn (show (fst df3))
       putStrLn (show (snd df3))
       putStrLn (show (uncurry encodeFloat df1 :: Float))
       putStrLn (show (uncurry encodeFloat df2 :: Float))
       putStrLn (show (uncurry encodeFloat df3 :: Float))

