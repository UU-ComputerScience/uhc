{-# OPTIONS -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  LR0
-- Author      :  Gerrit van den Geest 
-- E-mail      :  ggeest@cs.uu.nl
-- Date        :  07-02-2005
-- 
-- http://www.cs.uu.nl/wiki/Stc/TypedQuoteAntiquote
--
-- From:
--
-- * Ralf Hinze:
--   Functional Pearl: Typed Quote/Antiquote
--   Under consideration for publication in J. Functional Programming
--   http://www.informatik.uni-bonn.de/~ralf/publications/Quote.pdf
--
-- Concrete syntax for language of balanced parentheses.
--
-- p == (
-- q == )
-----------------------------------------------------------------------------
module LR0
where

import CPS


-- p == (
-- q == )
test = quote p  p q  p   p q   p q  q q end


data Tree = Leaf
          | Fork Tree Tree
          deriving Show

data S1    = S1          -- S
data S2 st = S2 st Tree  -- P
data S3 st = S3 st       -- end
data S4 st = S4 st Tree  -- P
data S5 st = S5 st       -- p
data S6 st = S6 st       -- q


shift = lift

state1 st             = state2 (S2 st Leaf)
state2                = shift
state3 (S3 (S2 S1 t)) = t
state4                = shift
state5 st             = state4 (S4 st Leaf)

class State6 old new | old -> new where
  state6 :: old -> CPS new
   
instance State6 (S6 (S4 (S5 (S2 S1)))) (S2 S1) where
  state6 (S6 (S4 (S5 (S2 S1 t)) u)) = state2 (S2 S1 (Fork t u))

instance State6 (S6 (S4 (S5 (S4 (S5 st))))) (S4 (S5 st)) where
  state6 (S6 (S4 (S5 (S4 (S5 st) t)) u)) = state4 (S4 (S5 st) (Fork t u))
  
quote = state1 S1  


class Open old new | old -> new where
  p :: old -> CPS new
  
instance Open (S2 st) (S4 (S5 (S2 st))) where
  p st@(S2 _ _) = state5 (S5 st)
  
instance Open (S4 st) (S4 (S5 (S4 st))) where
  p st@(S4 _ _) = state5 (S5 st)
  
q st@(S4 _ _) = state6 (S6 st)

end st@(S2 _ _) = state3 (S3 st)
  
