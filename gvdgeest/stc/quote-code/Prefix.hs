{-# OPTIONS -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Prefix
-- Author      :  Gerrit van den Geest 
-- E-mail      :  ggeest@cs.uu.nl
-- Date        :  07-02-2005
-- 
-- http://www.cs.uu.nl/wiki/Stc/TypedQuoteAntiquote
--
-- Inspired by:
--
-- * Ralf Hinze:
--   Functional Pearl: Typed Quote/Antiquote
--   Under consideration for publication in J. Functional Programming
--   http://www.informatik.uni-bonn.de/~ralf/publications/Quote.pdf
--                 
-- * Chris Okasaki:
--   Techniques for embedding postfix languages in Haskell
--   Haskell '02: Proceedings of the 2002 ACM SIGPLAN workshop on Haskell
--
-- * Simon L. Peyton Jones:
--   Composing Contracts: An Adventure in Financial Engineering
--   FME '01: Proceedings of the International Symposium of Formal Methods 
--   Europe on Formal Methods for Increasing Software Productivity
--
-- Concrete syntax for prefix notation for Financial Contracts.
-----------------------------------------------------------------------------
module Prefix
where
import CPS
import Prelude(Show, id)

{----------------------------------------------------------

2.1 Prefix =Parsing data types=

Prefix of Polish notation. In prefix notation a function 
precedes its arguments, so the state becomes a stack of
pending arguments.



-----------------------------------------------------------}

contract   =    quote
                   give or zero one
                end
                
data Tree  = Leaf | Fork Tree Tree deriving Show

fork' ctx = \t1 t2 -> ctx (Fork t1 t2)

leaf' ctx = ctx Leaf

fork = lift fork'
leaf = lift leaf'

data Contract = Zero
              | One
              | Or Contract Contract
              | Give Contract
             deriving Show


quote    ::   CPS    (alpha -> alpha)
quote    =    lift   id

end      ::   alpha   ->    alpha
end           a       =     a

zero   ::   (Contract -> a)   ->   CPS     a
zero        ctx               =    lift    (ctx Zero)

one    ::   (Contract -> a)   ->   CPS     a
one         ctx               =    lift    (ctx One)

give   ::   (Contract -> a)   ->   CPS     (Contract -> a)
give        ctx               =    lift    (\c -> ctx (Give c))

or     ::   (Contract -> a)   ->   CPS     (Contract -> Contract -> a)
or          ctx               =    lift    (\c1 c2 -> ctx (Or c1 c2))
