module Main where

foreign import ccall primAddInt :: Int -> Int -> Int

inc :: Int -> Int
inc x = primAddInt x 1

add = primAddInt

f x = add x x

main = f (inc 41)

