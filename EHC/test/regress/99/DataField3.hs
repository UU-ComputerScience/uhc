{- ----------------------------------------------------------------------------------------
   what    : duplicate data field
   expected: not ok, because different types
---------------------------------------------------------------------------------------- -}

module DataField3 where

data Test a
  = One { first :: a }
  | Two { first :: Int }

main = return ()
