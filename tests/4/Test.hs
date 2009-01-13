module Main where

foreign import ccall primAddInt :: Int -> Int -> Int

inc :: Int -> Int
inc x = primAddInt x 1

data List =
    Nil
  | Cons Int List

length :: List -> Int
length = f 0
  where
    f x Nil         = x
    f x (Cons y ys) = f (inc x) ys

five :: List
five = Cons 1 (Cons 2 (Cons 3 (Cons 4 (Cons 5 Nil))))

main = length five

