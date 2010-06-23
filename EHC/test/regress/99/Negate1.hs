{- ----------------------------------------------------------------------------------------
   what    : '-' (negate) at various positions.
   expected: ok, should bind tightest to application fexp, not aexp
             this differs slightly from report (see pg 139)
---------------------------------------------------------------------------------------- -}

module Negate1 where

main :: IO ()
main
  = do let x = 4 :: Int
           y = 5 :: Int
       putStrLn (show (x + -y))
       putStrLn (show (-x + y))
       putStrLn (show (x - -y))
       putStrLn (show (-x - y))
       putStrLn (show (x * -y))
       putStrLn (show (-x * y))
       putStrLn (show (- id x))
       putStrLn (show (id (- x)))
       putStrLn (show ((+ -x) y))
       putStrLn (show ((-x) + y))
