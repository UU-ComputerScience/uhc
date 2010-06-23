{- ----------------------------------------------------------------------------------------
   what    : Integral class, for Integer
   expected: ok
---------------------------------------------------------------------------------------- -}

module IntegralInteger1 where

main :: IO ()
main
  = do putStrLn (show ((5::Integer) `quot` 3))
       putStrLn (show ((5::Integer) `rem` 3))
       let (q,r) = quotRem (5::Integer) (3)
       putStrLn (show q)
       putStrLn (show r)
       putStrLn ""
       
       putStrLn (show ((-5::Integer) `quot` 3))
       putStrLn (show ((-5::Integer) `rem` 3))
       let (q,r) = quotRem (-5::Integer) (3)
       putStrLn (show q)
       putStrLn (show r)
       putStrLn ""

       putStrLn (show ((5::Integer) `quot` (-3)))
       putStrLn (show ((5::Integer) `rem` (-3)))
       let (q,r) = quotRem (5::Integer) (-3)
       putStrLn (show q)
       putStrLn (show r)
       putStrLn ""

       putStrLn (show ((-5::Integer) `quot` (-3)))
       putStrLn (show ((-5::Integer) `rem` (-3)))
       let (q,r) = quotRem (-5::Integer) (-3)
       putStrLn (show q)
       putStrLn (show r)
       putStrLn ""

       putStrLn (show ((5::Integer) `div` 3))
       putStrLn (show ((5::Integer) `mod` 3))
       let (d,m) = divMod (5::Integer) (3)
       putStrLn (show d)
       putStrLn (show m)
       putStrLn ""
 
       putStrLn (show ((-5::Integer) `div` 3))
       putStrLn (show ((-5::Integer) `mod` 3))
       let (d,m) = divMod (-5::Integer) (3)
       putStrLn (show d)
       putStrLn (show m)
       putStrLn ""
 
       putStrLn (show ((5::Integer) `div` (-3)))
       putStrLn (show ((5::Integer) `mod` (-3)))
       let (d,m) = divMod (5::Integer) (-3)
       putStrLn (show d)
       putStrLn (show m)
       putStrLn ""
 
       putStrLn (show ((-5::Integer) `div` (-3)))
       putStrLn (show ((-5::Integer) `mod` (-3)))
       let (d,m) = divMod (-5::Integer) (-3)
       putStrLn (show d)
       putStrLn (show m)
       
       
