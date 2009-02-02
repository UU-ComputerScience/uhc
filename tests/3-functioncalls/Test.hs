module Main where

foreign import ccall "primAddInt" (+) :: Int -> Int -> Int

inc :: Int -> Int
inc x = x + 1

data List =
    Nil
  | Cons Int List

length :: List -> Int
length Nil         = 0
length (Cons x xs) = inc (length xs)

five :: List
five = Cons 1 (Cons 2 (Cons 3 (Cons 4 (Cons 5 Nil))))

main = length five

