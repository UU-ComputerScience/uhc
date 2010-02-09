{- ----------------------------------------------------------------------------------------
   what    : duplicate data field
   expected: ok
---------------------------------------------------------------------------------------- -}

module DataField1 where

data Test a
  = One { first :: a }
  | Two { first :: a }

main = putStrLn (show $ first (One (1::Int)) + first (Two (2::Int)))
