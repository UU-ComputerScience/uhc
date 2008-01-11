{-
 - This is a EHC modified version of th wheel-sieve2 benchmark of the nofib/imaginary
 - Original code by Colin Runciman (colin@cs.york.ac.uk)
 - Changes to be compiled by EHC: John van Schie
 -}
{-
infixl 9  !!
infixl 7  *,  `mod`
infixl 6  +, -
infixr 5  :
infixr 5  ++
infix  4  ==, /=, <, <=, >, `elem`, `notElem`
infixr 3  &&
infixr 2  ||


foreign import ccall "primAddInt" (+)   :: Int -> Int -> Int
foreign import ccall "primSubInt" (-)   :: Int -> Int -> Int
foreign import ccall "primMulInt" (*)   :: Int -> Int -> Int
foreign import ccall "primModInt" mod   :: Int -> Int -> Int

foreign import ccall "primLtInt"  (<)   :: Int -> Int -> Bool
foreign import ccall "primGtInt"  (>)   :: Int -> Int -> Bool
foreign import ccall "primEqInt"  (==)  :: Int -> Int -> Bool

data PackedString
foreign import ccall "primCStringToString" packedStringToString :: PackedString -> [Char]

error :: [Char] -> a
error s = undefined
undefined :: forall a . a
undefined = error "undefined"

data Bool = False | True
data ''[]'' a = a : [a] | ''[]'' 

const          :: a -> b -> a
const k x      = k

head (x:xs) = x

tail (x:xs) = xs

span                 :: (a -> Bool) -> [a] -> ([a],[a])
span p []            = ([],[])
span p xs@(x:xs')
         | p x       = (x:ys, zs)
         | True      = ([],xs)
                       where (ys,zs) = span p xs'


dropWhile           :: (a -> Bool) -> [a] -> [a]
dropWhile p []       = []
dropWhile p xs@(x:xs')
         | p x       = dropWhile p xs'
         | True      = xs

zipWith                  :: (a->b->c) -> [a]->[b]->[c]
zipWith z (a:as) (b:bs)   = z a b : zipWith z as bs
zipWith z as      bs      = []

zipWith3                 :: (a->b->c->d) -> [a]->[b]->[c]->[d]
zipWith3 z (a:as) (b:bs) (c:cs)
                          = z a b c : zipWith3 z as bs cs
zipWith3 z as bs bc       = []


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
-}
{- wheel-sieve2 code -}

primes :: [Int]
primes = spiral wheels primes squares

spiral :: [Wheel] -> [a] -> [Int] -> [Int]
spiral (Wheel s ms ns:ws) ps qs =
  foldr turn0 (roll s) ns
  where
  roll o = foldr (turn o) (foldr (turn o) (roll (o+s)) ns) ms
  turn0  n rs =
    if n<q then n:rs else sp
  turn o n rs =
    let n' = o+n in
    if n'==2 || n'<q then n':rs else dropWhile (<n') sp
  sp = spiral ws (tail ps) (tail qs)
  q = head qs

squares :: [Int]
squares = [p*p | p <- primes]

data Wheel = Wheel Int [Int] [Int]

wheels :: [Wheel]
wheels = Wheel 1 [1] [] :
         zipWith3 nextSize wheels primes squares 

nextSize :: Wheel -> Int -> Int -> Wheel
nextSize (Wheel s ms ns) p q =
  Wheel (s*p) ms' ns'
  where
  (xs, ns') = span (<=q) (foldr turn0 (roll (p-1) s) ns)
  ms' = foldr turn0 xs ms
  roll t o | t==0  = []
           | True  = foldr (turn o) (foldr (turn o) (roll (t-1) (o+s)) ns) ms
  turn0  n rs =
    if n`mod`p>0 then n:rs else rs
  turn o n rs =
    let n' = o+n in
    if n'`mod`p>0 then n':rs else rs

arg  = <ARG1>
main = <PRINT_INT> primes !! arg
