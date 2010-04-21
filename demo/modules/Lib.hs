module Lib where

number :: N
number = S (S (S (S Z)))

id :: a -> a
id x = x

($) :: (a -> b) -> a -> b
($) = id

data N = Z | S N

(+) :: N -> N -> N
Z     + n     = n
m     + Z     = m
(S m) + n     = S (m + n)
m     + (S n) = S (m + n)

undefined :: a
undefined = undefined

