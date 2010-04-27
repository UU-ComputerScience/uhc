{- ----------------------------------------------------------------------------------------
   what    : Recursive use of class in constraint
   expected: ok
---------------------------------------------------------------------------------------- -}

module ClassRec1 where

class Data a where
  gfoldl :: (forall d. Data d => d) -> a

main
  = return ()
