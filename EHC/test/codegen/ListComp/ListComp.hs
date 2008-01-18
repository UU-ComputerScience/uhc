
data Bool = False | True
data ''[]'' a = a : [a] | ''[]''

foreign import ccall "primAddInt" (+)  :: Int -> Int -> Int
foreign import ccall "primSubInt" (-)  :: Int -> Int -> Int
foreign import ccall "primEqInt" (==) :: Int -> Int -> Bool
foreign import ccall "primGtInt" (>)  :: Int -> Int -> Bool
foreign import ccall primNegInt     :: Int -> Int

negate = primNegInt

(++) :: [a] -> [a] -> [a]
[]     ++ ys = ys
(x:xs) ++ ys = x : (xs ++ ys)

concat :: [[a]] -> [a]
concat []     = []
concat (x:xs) = x ++ (concat xs)

concatMap :: (a -> [b]) -> [a] -> [b]
concatMap f []     = []
concatMap f (x:xs) = (f x) ++ (concatMap f xs) 

enumFromTo :: Int -> Int -> [Int]
enumFromTo m n | m > n = [] 
               | True  = m : enumFromTo (m + 1) n

length :: [a] -> Int
length []     = 0
length (x:xs) = length xs + 1

abs x
  | x > 0  = x
  | x == 0 = x
  | True   = negate x

main = length [ (x,y) | x <- [1..4], y <- [1..8], test (x,y)]

test (x,y) = (abs (x - y)) == 2
