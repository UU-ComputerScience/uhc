module Main where

foreign import ccall primAddInt :: Int -> Int -> Int

inc :: Int -> Int
inc x = primAddInt x 1

add = primAddInt

f x y z = add z x

g x = f 3 x

main = g (inc 41) 4

