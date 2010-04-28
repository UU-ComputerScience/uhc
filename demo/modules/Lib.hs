module Lib where

number :: N
number = S (S (S (S Z)))

data N = Z | S N

class Succ a where
  succ :: a -> a

undefined :: a
undefined = undefined

