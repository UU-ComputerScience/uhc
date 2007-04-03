{-# OPTIONS -fglasgow-exts #-}
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
module CPS (lift, CPS)  where


type CPS alpha = forall r . (alpha -> r) -> r

lift ::   alpha   ->    CPS alpha
lift      a       =     \f -> f a

quote   ::              CPS    Int
quote              =    lift   0

tick ::      Int   ->   CPS    Int
tick         i     =    lift   (i + 1)

end ::       Int   ->   Int
end          i     =    i

test3 = quote tick tick tick end

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


{--------------------------------------------------------
This technique of passing control to a function argument is
reminiscent of continuation-passing style.

http://www.nomaware.com/monads/html/contmonad.html
http://en.wikipedia.org/wiki/Continuation_passing_style

	Continuation-passing Haskell programmer
	facCps k 0 = k 1
	facCps k n = facCps (k . (n *)) (n-1)

	fac = facCps id

Default Monad CPS instance

	instance Monad CPS where
	   return a = CPS (\k -> k a) 
	   m >>= k  = CPS (\c -> unCPS m (\a -> unCPS (k a) c))   

We would like for this application that we can create instances
of higher kinded type-synonyms.
	   
http://www.haskell.org/pipermail/haskell/2000-October/006128.html	   
-------------------------------------------------------------}

data CPS2 a = CPS2 (forall r. (a -> r) -> r)

unCPS (CPS2 f) = f

instance Monad CPS2 where
  return a      = CPS2 (\k -> k a)
  (CPS2 m) >>= k = CPS2 (m (\a -> unCPS (k a)))

type Arrow2 a b = a -> CPS2 b

lift2 :: (a -> b) -> Arrow2 a b
lift2 f = return . f

lift3 :: (a -> b) -> Arrow2 a b
lift3 f g = return ( f  g)
  
run :: CPS2 a -> a
run (CPS2 m) = m id

mquote :: CPS2 [Int]
mquote = return []


mtick :: Arrow2 Int Int
mtick = lift2 succ

