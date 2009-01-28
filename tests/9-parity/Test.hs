module Main where

foreign import ccall "primSubInt" (-)  :: Int -> Int -> Int
foreign import ccall "primEqInt" (==) :: Int -> Int -> Bool

data Bool = True | False

even :: Int -> Bool
even 0 = True
even x = odd (x - 1)

odd :: Int -> Bool
odd 0 = False
odd x = even (x - 1)

show :: Bool -> Int
show True  = 1
show False = 0

main = show (even 100000000)

