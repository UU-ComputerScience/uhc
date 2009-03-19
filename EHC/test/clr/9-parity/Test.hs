module Main where

data Bool = False | True

foreign import ccall "primSubInt" (-)  :: Int -> Int -> Int
foreign import ccall "primEqInt" (==) :: Int -> Int -> Bool

even :: Int -> Bool
even x | x == 0 = True
       | True   = odd (x - 1)

odd :: Int -> Bool
odd x | x == 0 = False
      | True   = even (x - 1)

main = even 100000000

