

module Main where

foreign import ccall "primAddInt" (+) :: Int -> Int -> Int
foreign import ccall "primGtInt" (>) :: Int -> Int -> Bool

data Bool = True | False

data ''[]'' a = a : [a] | ''[]''

infixr 6 :

main = sum (upto 1 5)  -- 100000

upto m n = if m > n then [] else m : upto (m + 1) n

sum l = case l of
    []     -> 0
    (n:ns) -> n + sum ns



