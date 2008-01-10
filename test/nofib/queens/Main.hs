{-
 - This is a EHC modified version of th primes benchmark of the nofib/imaginary
 - Original code by (grabbed from LML dist)
 - Changes to be compiled by EHC: John van Schie
 -}

foreign import ccall "primAddInt" (+)   :: Int -> Int -> Int
foreign import ccall "primSubInt" (-)   :: Int -> Int -> Int
foreign import ccall "primEqInt" (==)   :: Int -> Int -> Bool
foreign import ccall "primNeInt" (/=)   :: Int -> Int -> Bool
foreign import ccall "primGtInt" (>)    :: Int -> Int -> Bool

data Bool = False | True
data ''[]'' a = a : [a] | ''[]''

{- Library functions -}
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ n []     = n
foldr f n (x:xs) = f x (foldr f n xs)


length :: [a] -> Int
length []     = 0
length (x:xs) = 1 + length xs

(++) :: [a] -> [a] -> [a]
(++) []     ys = ys
(++) (x:xs) ys = x : (xs ++ ys)

map :: (a -> b) -> [a] -> [b]
map f []     = []
map f (x:xs) = f x : (map f xs)

concat :: [[a]] -> [a]
concat = foldr (++) []

concatMap :: (a -> [b]) -> [a] -> [b]
concatMap f xs = concat ( map f xs )


enumFromTo :: Int -> Int -> [Int]
enumFromTo m n | m > n = []
               | True  = m : enumFromTo (m + 1) n

(&&) :: Bool -> Bool -> Bool
x && y =
  if x then y else False

{- Queen code -}
arg = 12

main = nsoln arg

nsoln nq = length (gen nq)
 where
    safe :: Int -> Int -> [Int] -> Bool
    safe x d []    = True
    safe x d (q:l) = (x /= q) && (x /= (q+d)) && (x /= (q-d)) && (safe x (d+1) l)

    gen :: Int -> [[Int]]
    gen 0 = [[]]
    gen n = [ (q:b) | b <- gen (n-1), q <- [1..nq], safe q 1 b]
