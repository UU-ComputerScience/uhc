{- ----------------------------------------------------------------------------------------
   what    : duplicate data field
   expected: not ok, because in different datatypes
---------------------------------------------------------------------------------------- -}

module DataField2 where

data Test a
  = One { first :: a }
  | Two { first :: a }

data Test2 a
  = Three { first :: a }

main = return ()
