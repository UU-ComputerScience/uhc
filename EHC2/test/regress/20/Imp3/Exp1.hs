-- explicit export: class+instance

module Imp3.Exp1
  ( C(..), dCInt
  )
  where

class C a where
  c :: a -> a

instance dCInt <: C Int where
  c x = x

{-
instance C Int where
  c x = x
-}