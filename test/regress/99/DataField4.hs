{- ----------------------------------------------------------------------------------------
   what    : correct generation of field extraction in the presence of type params (bug fix)
   expected: ok
---------------------------------------------------------------------------------------- -}

module DataField4 where

data Sum a = L a | R { unR :: a }

main = return ()
