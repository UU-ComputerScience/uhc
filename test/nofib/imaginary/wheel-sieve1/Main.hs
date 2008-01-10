{-
 - This is a EHC modified version of th wheel-sieve1 benchmark of the nofib/imaginary
 - Original code by Colin Runciman (colin@cs.york.ac.uk)
 - Changes to be compiled by EHC: John van Schie
 -}

foreign import ccall "primAddInt" (+)   :: Int -> Int -> Int
foreign import ccall "primSubInt" (-)   :: Int -> Int -> Int
foreign import ccall "primMulInt" (*)   :: Int -> Int -> Int
foreign import ccall "primModInt" mod   :: Int -> Int -> Int

foreign import ccall "primGtInt"  (>)   :: Int -> Int -> Bool
foreign import ccall "primEqInt"  (==)  :: Int -> Int -> Bool

data Bool = False | True
data ''[]'' a = a : [a] | ''[]'' 

const          :: a -> b -> a
const k x      = k

head (x:xs) = x

tail (x:xs) = xs

zipWith                  :: (a->b->c) -> [a]->[b]->[c]
zipWith z (a:as) (b:bs)   = z a b : zipWith z as bs
zipWith z as      bs      = []

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ n []     = n
foldr f n (x:xs) = f x (foldr f n xs)

enumFromTo :: Int -> Int -> [Int]
enumFromTo m n | m > n = []
               | True  = m : enumFromTo (m + 1) n

enumFromThenTo x y z =
  [x, y .. z]

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

(!!) :: [a] -> Int -> a
(x:xs) !! i = if i == 0
              then x
              else xs !! (i - 1)

(&&) :: Bool -> Bool -> Bool
x && y =
  if x then y else False

(||) :: Bool -> Bool -> Bool
x || y =
  if x then True else y

(<=) :: Int -> Int -> Bool
x <= y =
  if x > y
  then False
  else True

{- wheel-sieve1 code -}
data Wheel = AWheel Int [Int]

primes :: [Int]
primes = sieve wheels primes squares

sieve :: [Wheel] -> [Int] -> [Int] -> [Int]
sieve ((AWheel s ns):ws) ps qs =
  [n' | o <- s:[s*2,s*3..(head ps-1)*s],
        n <- ns,
        n'<- [n+o], noFactor n'] 
  ++
  sieve ws (tail ps) (tail qs)
  where
  noFactor = if (s <= 2) then const True else notDivBy ps qs

notDivBy :: [Int] -> [Int] -> Int -> Bool
notDivBy (p:ps) (q:qs) n =
  (q > n) || (((n `mod` p) > 0) && (notDivBy ps qs n))

squares :: [Int]
squares = [p*p | p<-primes]

wheels :: [Wheel]
wheels = (AWheel 1 [1]) : zipWith nextSize wheels primes 

nextSize :: Wheel -> Int -> Wheel
nextSize (AWheel s ns) p =
  AWheel (s*p) ns'
  where
  ns' = [n' | o <- [0,s..((p-1)*s)],
              n <- ns,
              n' <- [n+o], n'`mod`p > 0]

arg  = 230000
main =  primes!!arg
