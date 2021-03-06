{- ----------------------------------------------------------------------------------------
   what    : Bounded class, derived (via generics)
   expected: ok
   platform: word size dependent
---------------------------------------------------------------------------------------- -}

module BoundedDeriv1 where

import UHC.Generics

data E = E1 | E2 | E3 | E4
  deriving (Show,Bounded,Generic)

data F = F1 Int Char | F2 Bool E
  deriving (Show,Bounded,Generic)

main :: IO ()
main
  = do print (minBound :: E)
       print (maxBound :: E)
       print (minBound :: F)
       print (maxBound :: F)
