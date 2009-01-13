module Main where

foreign import ccall primAddInt :: Int -> Int -> Int

inc :: Int -> Int
inc x = primAddInt x 1

main = inc 41

