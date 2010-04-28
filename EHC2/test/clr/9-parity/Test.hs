module Main where

infixr 0  $, $!, `seq`

seq :: forall a b . a -> b -> b
x `seq` y = letstrict x' = x in y

($!) :: (a -> b) -> a -> b
f $! x                = x `seq` f x

data Bool = False | True

foreign import ccall "primSubInt" (-)  :: Int -> Int -> Int
foreign import ccall "primEqInt" (==) :: Int -> Int -> Bool

even :: Int -> Bool
even x | x == 0 = True
       | True   = odd $! x - 1

odd :: Int -> Bool
odd x | x == 0 = False
      | True   = even $! x - 1

main = even 100000000

