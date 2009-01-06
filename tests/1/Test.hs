module Main where

foreign import ccall primAddInt :: Int -> Int -> Int

inc :: Int -> Int
inc x = primAddInt x 1

data MyData =
    One Int
  | Two Int Int

main = inc 7

