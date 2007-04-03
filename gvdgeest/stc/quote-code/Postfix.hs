{-# OPTIONS -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Postfix
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
-- Concrete syntax for postfix notation for Financial Contracts
-----------------------------------------------------------------------------
module Postfix
where
import Prelude (Show)
import CPS

{----------------------------------------------------------

2.1 Postfix =Parsing data types=

Postfix notation, evaluation of postfix expressions is
stack-based. Functions pops there arguments from the stack
and pash their result.

A data constructor C of type t1 -> ... -> tn -> t is
translaterd to 

c :: (((st, t1), ...), tn) -> (st, t)

-----------------------------------------------------------}
contract   =    quote
                   one zero or give
                end


data Contract = Zero
              | One
              | Give Contract
              | Or   Contract Contract
              deriving Show

quote    ::   CPS    ()
quote    =    lift   ()

end      ::   ((), alpha)  ->    alpha
end           ((), a    )  =     a


zero    ::  st                                    ->    CPS    (st, Contract)
zero        st                                    =     lift   (st, Zero)

one     ::  st                                    ->    CPS    (st, Contract)
one         st                                    =     lift   (st, One)

give    ::  (st, Contract   )                     ->    CPS    (st, Contract)
give        (st, c          )                     =     lift   (st, Give c)

or      ::  ((st, Contract   ),   Contract   )    ->    CPS    (st, Contract)
or          ((st, c1         ),   c2         )    =     lift   (st, Or c1 c2)