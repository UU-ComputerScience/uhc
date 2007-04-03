-----------------------------------------------------------------------------
-- |
-- Module      :  CPS
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
-- This module exports the functions needed to parse concrete syntax in 
-- Haskell.
--
-----------------------------------------------------------------------------

type CPS alpha r = (alpha -> r) -> r

lift ::   alpha   ->    CPS alpha r
lift      a       =     \f -> f a

quote   ::              CPS    Int a
quote              =    lift   0

tick ::      Int   ->   CPS    Int a
tick         i     =    lift   (i + 1)

end ::       Int   ->   Int
end          i     =    i

main = print $ largetest

largetest = quote tick tick tick tick tick tick tick tick tick tick tick tick tick tick tick tick tick tick  tick  tick  tick  tick  tick  tick  tick  tick  tick  tick  tick  tick  tick  tick  tick  tick tick tick tick tick tick tick tick tick tick tick tick tick tick tick tick tick tick tick  tick  tick  tick  tick  tick  tick  tick  tick  tick  tick  tick  tick  tick  tick  tick  tick  tick tick tick tick tick tick tick tick tick tick tick tick tick tick tick tick tick tick  tick  tick  tick  tick  tick  tick  tick  tick  tick  tick  tick  tick  tick  tick  tick  tick tick tick tick tick tick tick tick tick tick tick tick tick tick tick tick tick tick tick  tick  tick  tick  tick  tick  tick  tick  tick  tick  tick  tick  tick  tick  tick  tick  tick  end

{--------------------------------------------------------
Stepwise evaluation, tick increments the counter and then 
passes control to the next terminal:

quote tick tick tick enquote
tick 0 tick tick enquote
tick 1 tick enquote
tick 2 enquote
enquote 3
3
---------------------------------------------------------}

test =   quote
            tick
            tick
            tick
         end
         +
         quote
            tick
         end
  
{-----------------------------------------------------
2. =Background: The other CPS monad=

Concrete syntax for naturals, it looks like a sequence of
tick, but it is a nested function application.

If Haskell used postfix (reverse polish) function 
application we could set
------------------------------------------------------}

quote'   ::              Int
quote'              =    0

tick' ::      Int   ->   Int
tick'         i     =    i + 1

end' ::       Int   ->   Int
end'          i     =    i

{----------------------------------------------------------
For Hakells prefix function-application we must arrange that
functions and arguments are swapped:
----------------------------------------------------------}

infixl 0 <|

(<|) :: alpha -> (alpha -> r) -> r

x <| f = f x

test2  = quote' <| tick' <| tick' <| tick' <| end'
