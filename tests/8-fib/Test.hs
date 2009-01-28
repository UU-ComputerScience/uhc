module Main where

data Bool = False | True

foreign import ccall "primAddInt" (+)  :: Int -> Int -> Int
foreign import ccall "primSubInt" (-)  :: Int -> Int -> Int
foreign import ccall "primEqInt" (==) :: Int -> Int -> Bool

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

-- fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

main = fib 4

