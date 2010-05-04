{- ----------------------------------------------------------------------------------------
   what    : type level operator syntax
   expected: all ok
---------------------------------------------------------------------------------------- -}

module TypeOperator1 where

import Control.Arrow

data (:+:) a b = a :+: b
data a :*: b = a :*: b
data D1 (+) = I1 (Int  +  Int)
data D2  x  = I2 (Int  +  x  )
data D3  x  = I3 (Int `x` Int)
type T1 (+) = Int  +  Int
-- error:
-- type T2  x  = Int  +  x  
type T3  x  = Int `x` Int

x ::  Int `Either` Char
x = Left 8

liftA2 :: Arrow (~>) => (a -> b -> c) -> (e ~> a) -> (e ~> b) -> (e ~> c)
liftA2 = undefined

main = return ()
