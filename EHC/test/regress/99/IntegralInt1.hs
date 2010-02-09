{- ----------------------------------------------------------------------------------------
   what    : Integral class, for Int
   expected: ok
---------------------------------------------------------------------------------------- -}

module IntegralInt1 where

main :: IO ()
main
  = do putStrLn (show ((5::Int) `quot` 3))
       putStrLn (show ((5::Int) `rem` 3))
       let (q,r) = quotRem (5::Int) (3)
       putStrLn (show q)
       putStrLn (show r)
       putStrLn ""
       
       putStrLn (show ((-5::Int) `quot` 3))
       putStrLn (show ((-5::Int) `rem` 3))
       let (q,r) = quotRem (-5::Int) (3)
       putStrLn (show q)
       putStrLn (show r)
       putStrLn ""

       putStrLn (show ((5::Int) `quot` (-3)))
       putStrLn (show ((5::Int) `rem` (-3)))
       let (q,r) = quotRem (5::Int) (-3)
       putStrLn (show q)
       putStrLn (show r)
       putStrLn ""

       putStrLn (show ((-5::Int) `quot` (-3)))
       putStrLn (show ((-5::Int) `rem` (-3)))
       let (q,r) = quotRem (-5::Int) (-3)
       putStrLn (show q)
       putStrLn (show r)
       putStrLn ""

       putStrLn (show ((5::Int) `div` 3))
       putStrLn (show ((5::Int) `mod` 3))
       let (d,m) = divMod (5::Int) (3)
       putStrLn (show d)
       putStrLn (show m)
       putStrLn ""
 
       putStrLn (show ((-5::Int) `div` 3))
       putStrLn (show ((-5::Int) `mod` 3))
       let (d,m) = divMod (-5::Int) (3)
       putStrLn (show d)
       putStrLn (show m)
       putStrLn ""
 
       putStrLn (show ((5::Int) `div` (-3)))
       putStrLn (show ((5::Int) `mod` (-3)))
       let (d,m) = divMod (5::Int) (-3)
       putStrLn (show d)
       putStrLn (show m)
       putStrLn ""
 
       putStrLn (show ((-5::Int) `div` (-3)))
       putStrLn (show ((-5::Int) `mod` (-3)))
       let (d,m) = divMod (-5::Int) (-3)
       putStrLn (show d)
       putStrLn (show m)
       
       
