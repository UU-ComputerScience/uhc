module Main where

foreign import ccall primAddInt :: Int -> Int -> Int

inc :: Int -> Int
inc x = primAddInt x 1

data MyData =
    One   Int
  | Two   Int Int
  | Three Int Int Int
  | Four  Int Int Int Int

data Maybe a =
    Nothing
  | Just a

main = inc 7

