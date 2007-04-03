{-# OPTIONS -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  GreibachFinancial
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
-- Concrete syntax for Financial Combinators, described by an grammar in GNF
-----------------------------------------------------------------------------
module GreibachFinancial where

import CPS
import Prelude (id, Show, Maybe (Just, Nothing), (.), ($), Double)

{----------------------------------------------------------

Start     = quote Contract Contracts

Contracts = or    Contract Contracts
          | end

Contract  = give Contract
          | one  Currency
          | zero

Currency  = dollar
          | euro

-----------------------------------------------------------}

contract =
          quote
                 give one dollar    
            or   give one euro
            or   give zero
          end

type Contracts = [Contract]

data Contract = Zero 
              | Give  Contract
              | One   Currency 
              | Scale Double   Contract 
              deriving Show
            
data Currency = Euro
              | Dollar            
              deriving Show

newtype S  a = S  (Contracts -> a)
newtype C  a = C  (Contract  -> a)
newtype CS a = CS (Contracts -> a)
newtype CU a = CU (Currency  -> a)

quote  ::                   CPS     (C (CS ([Contract])))
quote                  =    lift    (C (\c -> CS (\cs -> c:cs)))

or     ::   (CS a)     ->   CPS     (C (CS a))
or          (CS ctx)   =    lift    (C (\c -> CS (\cs -> ctx (c:cs))))

end    ::   (CS a)     ->   a
end         (CS ctx)   =    ctx []

zero   ::   (C a)      ->   CPS     a
zero        (C  ctx)   =    lift    (ctx Zero)

one    ::   (C a)      ->   CPS     (CU a)
one         (C  ctx)   =    lift    (CU (\cur -> ctx (One cur)))

give   ::   (C a)      ->   CPS     (C a)
give        (C  ctx)   =    lift    (C  (\c   -> ctx (Give c )))

scale       (C  ctx) d =    lift    (C  (\c   -> ctx (Scale d  c)))

dollar ::   (CU a)     ->   CPS     a
dollar      (CU ctx)   =    lift   (ctx Dollar)

euro   ::   (CU a)     ->   CPS     a
euro        (CU ctx)   =    lift   (ctx Euro  )



