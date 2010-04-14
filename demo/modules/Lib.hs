module Lib where

number :: Int
number = 4

id :: a -> a
id x = x

($) :: (a -> b) -> a -> b
($) = id

