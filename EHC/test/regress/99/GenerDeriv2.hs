{- ----------------------------------------------------------------------------------------
   what    : generic deriving: to0/from0 conversion
   expected: ok
---------------------------------------------------------------------------------------- -}

module GenerDeriv2 where

import UHC.Generics

data Exp = Const Int | Exp :+++: Exp | II {ii::Int} -- | JJ y x
  deriving Show

y = to0 (from0 (II 5 :+++: II 4 :+++: Const 6) :: _Rep0Exp z) :: Exp

main = putStrLn (show y)

