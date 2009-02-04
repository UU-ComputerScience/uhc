module Main where

data Bool = False | True

data ''[]'' a = a : [a] | ''[]''

foreign import ccall "primAddInt" (+) :: Int -> Int -> Int
foreign import ccall "primSubInt" (-) :: Int -> Int -> Int
foreign import ccall "primEqInt" (==) :: Int -> Int -> Bool

infixr 6 :

zipWith :: (Int -> Int -> Int) -> [Int] -> [Int] -> [Int]
zipWith _ _      _      = []
zipWith f (x:xs) (y:ys) = f x y : zipWith f xs ys

sum :: [Int] -> Int
sum []     = 0
sum (x:xs) = x + sum xs

tail :: [Int] -> [Int]
tail []     = []
tail (x:xs) = xs

take :: Int -> [Int] -> [Int]
take _ []     = []
take n (x:xs) = if x == 0
                then []
                else x : take (n-1) xs


fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

main = fibs -- sum (take 7500 fibs)

