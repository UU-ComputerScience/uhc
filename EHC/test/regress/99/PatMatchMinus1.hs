{- ----------------------------------------------------------------------------------------
   what    : bugfix triggered: '-' in pattern with constructors, ambiguity with - <literal>
   expected: ok
---------------------------------------------------------------------------------------- -}

module PatMatchMinus1 where

data Pos = Pos Int

Pos m + Pos n = undefined
Pos m - Pos n = undefined

main = return ()

