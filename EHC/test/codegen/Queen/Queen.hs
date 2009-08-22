{- EHC Only -}
data Bool = False | True
data ''[]'' a = a : [a] | ''[]''

foreign import ccall "primAddInt" (+) :: Int -> Int -> Int
foreign import ccall "primSubInt" (-) :: Int -> Int -> Int

foreign import ccall primNegInt :: Int -> Int
negate = primNegInt

foreign import ccall "primEqInt" (==) :: Int -> Int -> Bool
foreign import ccall "primNeInt" (/=) :: Int -> Int -> Bool
foreign import ccall "primGtInt" (>)  :: Int -> Int -> Bool

-- Lists
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ n []     = n
foldr f n (x:xs) = f x (foldr f n xs)

enumFromTo :: Int -> Int -> [Int]
enumFromTo m n | m > n = [] 
               | True  = m : enumFromTo (m + 1) n

all p xs       = and (map p xs)

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

length :: [a] -> Int
length []     = 0
length (x:xs) = 1 + length xs

-- Logical
not :: Bool -> Bool
not x =
  if x then False else True

(&&) :: Bool -> Bool -> Bool
x && y =
  if x then y else False

and xs = foldr (&&) True xs
-- Ints
abs x
  | x > 0 = x
  | True  = negate x

-- main for EHC
main = length (nqueens 12)

{- GHC Only -}
{-
main = putStr $ show $ length $ nqueens 8
-}
{- (c) http://programming.reddit.com/info/62j4m/comments/ by SamReidHughes -}
nqueens :: Int -> [[(Int,Int)]]
nqueens n = foldr qu [[]] [1..n]
    where qu k qss = [ ((j,k):qs) | qs <- qss, j <- [1..n], all (safe (j,k)) qs ]
          safe (j,k) (l,m) = (j /= l) && (k /= m) && (abs (j-l) /= abs (k-m))
