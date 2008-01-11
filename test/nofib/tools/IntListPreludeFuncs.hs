
-------------------------------------------------------------------------------
-- This is the EHC prelude for running the no-fib benchmarks with version 8  --
-------------------------------------------------------------------------------

infixr 9  .
infixl 9  !!
--infixr 8  ^, ^^, **
infixl 7  *, /, `div`, `mod`, `rem`, `quot`
infixl 6  +, -
infixr 5  :
infixr 5  ++
infix  4  ==, /=, <, <=, >=, >, `elem`, `notElem`
infixr 3  &&
infixr 2  ||
--infixl 1  >>, >>=
--infixr 1  =<<
infixr 0  $
--infixr 0  $, $!, `seq`


{------------------------------------------------------------------------------
-- Ints
-------------------------------------------------------------------------------}

-- Foreign imports
-- Arithmetic binary operators
foreign import ccall "primAddInt" (+)  :: Int -> Int -> Int
foreign import ccall "primSubInt" (-)  :: Int -> Int -> Int
foreign import ccall "primMulInt" (*)  :: Int -> Int -> Int

foreign import ccall "primDivInt" div  :: Int -> Int -> Int
-- Quot == Div for non negative numbers, let's assume that for now
foreign import ccall "primDivInt" quot  :: Int -> Int -> Int
foreign import ccall "primModInt" mod  :: Int -> Int -> Int
-- Rem == mod for non negative numbers, let's assume that for now
foreign import ccall "primModInt" rem   :: Int -> Int -> Int

-- Comparison binary operators
foreign import ccall "primEqInt" (==)   :: Int -> Int -> Bool
foreign import ccall "primNeInt" (/=)   :: Int -> Int -> Bool
foreign import ccall "primLtInt"  (<)   :: Int -> Int -> Bool
foreign import ccall "primGtInt"  (>)   :: Int -> Int -> Bool

-- Ordering
(<=) :: Int -> Int -> Bool
x <= y = not (x > y)

(>=) :: Int -> Int -> Bool
x >= y = not (x < y)

even, odd        :: Int -> Bool
even n           =  n `rem` 2 == 0
odd              =  not . even

gcd            :: Int -> Int -> Int
gcd x y         = gcd' (abs x) (abs y)
                  where gcd' x 0 = x
                        gcd' x y = gcd' y (x `rem` y)

abs          :: Int -> Int
abs x        | x >= 0    = x
             | otherwise = -x

signum       :: Int -> Int
signum x     | x == 0    =  0
             | x > 0     =  1
             | otherwise = -1

{------------------------------------------------------------------------------
-- Booleans
-------------------------------------------------------------------------------}
data Bool = False | True

(&&), (||)  :: Bool -> Bool -> Bool
False && x   = False
True  && x   = x
False || x   = x
True  || x   = True

not         :: Bool -> Bool
not x       = if x then False else True

otherwise   :: Bool
otherwise    = True
{-
and, or   :: [Bool] -> Bool
and        = foldr (&&) True
or         = foldr (||) False

any, all  :: (a -> Bool) -> [a] -> Bool
any p xs   = or  (map p xs)
all p xs   = and (map p xs)
-}
{------------------------------------------------------------------------------
-- Lists
-------------------------------------------------------------------------------}
data ''[]'' a = a : [a] | ''[]''

head             :: [a] -> a
head (x:_)        = x
{-
last             :: [a] -> a
last [x]          = x
last (_:xs)       = last xs
-}
tail             :: [a] -> [a]
tail (_:xs)       = xs
{-
init             :: [a] -> [a]
init [x]          = []
init (x:xs)       = x : init xs

null             :: [a] -> Bool
null []           = True
null (_:_)        = False
-}
(++)             :: [a] -> [a] -> [a]
[]     ++ ys      = ys
(x:xs) ++ ys      = x : (xs ++ ys)

map              :: (a -> b) -> [a] -> [b]
map f xs          = [ f x | x <- xs ]

filter           :: (a -> Bool) -> [a] -> [a]
filter p xs       = [ x | x <- xs, p x ]

concat           :: [[a]] -> [a]
concat            = foldr (++) []

length           :: [a] -> Int
length            = foldl (\n _ -> n + 1) 0

-- TODO, add errors when index is < 0 back in
(!!)             :: [a] -> Int -> a
(x:xs) !! n       = if n == 0
                    then x
                    else xs !! (n - 1)

foldl            :: (a -> b -> a) -> a -> [b] -> a
foldl f z []      = z
foldl f z (x:xs)  = foldl f (f z x) xs

foldl1           :: (a -> a -> a) -> [a] -> a
foldl1 f (x:xs)   = foldl f x xs
{-
scanl            :: (a -> b -> a) -> a -> [b] -> [a]
scanl f q xs      = q : (case xs of
                         []   -> []
                         x:xs -> scanl f (f q x) xs)

scanl1           :: (a -> a -> a) -> [a] -> [a]
scanl1 _ []       = []
scanl1 f (x:xs)   = scanl f x xs
-}
foldr            :: (a -> b -> b) -> b -> [a] -> b
foldr f z []      = z
foldr f z (x:xs)  = f x (foldr f z xs)
{-
foldr1           :: (a -> a -> a) -> [a] -> a
foldr1 f [x]      = x
foldr1 f (x:xs)   = f x (foldr1 f xs)

scanr            :: (a -> b -> b) -> b -> [a] -> [b]
scanr f q0 []     = [q0]
scanr f q0 (x:xs) = f x q : qs
                    where qs@(q:_) = scanr f q0 xs

scanr1           :: (a -> a -> a) -> [a] -> [a]
scanr1 f []       = []
scanr1 f [x]      = [x]
scanr1 f (x:xs)   = f x q : qs
                    where qs@(q:_) = scanr1 f xs
-}
iterate          :: (a -> a) -> a -> [a]
iterate f x       = x : iterate f (f x)
{-
repeat           :: a -> [a]
repeat x          = xs where xs = x:xs

replicate        :: Int -> a -> [a]
replicate n x     = take n (repeat x)

cycle            :: [a] -> [a]
cycle []          = error "Prelude.cycle: empty list"
cycle xs          = xs' where xs'=xs++xs'
-}
take                :: Int -> [a] -> [a]
take n _  | n <= 0  = []
take _ []           = []
take n (x:xs)       = x : take (n-1) xs
{-
drop                :: Int -> [a] -> [a]
drop n xs | n <= 0  = xs
drop _ []           = []
drop n (_:xs)       = drop (n-1) xs

splitAt               :: Int -> [a] -> ([a], [a])
splitAt n xs | n <= 0 = ([],xs)
splitAt _ []          = ([],[])
splitAt n (x:xs)      = (x:xs',xs'') where (xs',xs'') = splitAt (n-1) xs

takeWhile           :: (a -> Bool) -> [a] -> [a]
takeWhile p []       = []
takeWhile p (x:xs)
         | p x       = x : takeWhile p xs
         | otherwise = []
-}
dropWhile           :: (a -> Bool) -> [a] -> [a]
dropWhile p []       = []
dropWhile p xs@(x:xs')
         | p x       = dropWhile p xs'
         | otherwise = xs

span, break         :: (a -> Bool) -> [a] -> ([a],[a])
span p []            = ([],[])
span p xs@(x:xs')
         | p x       = (x:ys, zs)
         | otherwise = ([],xs)
                       where (ys,zs) = span p xs'
break p              = span (\x -> not (p x))

concatMap        :: (a -> [b]) -> [a] -> [b]
concatMap _ []      = []
concatMap f (x:xs)  = f x ++ concatMap f xs

zip              :: [a] -> [b] -> [(a,b)]
zip               = zipWith  (\a b -> (a,b))
{-
zip3             :: [a] -> [b] -> [c] -> [(a,b,c)]
zip3              = zipWith3 (\a b c -> (a,b,c))
-}
zipWith                  :: (a->b->c) -> [a]->[b]->[c]
zipWith z (a:as) (b:bs)   = z a b : zipWith z as bs
zipWith _ _      _        = []

zipWith3                 :: (a->b->c->d) -> [a]->[b]->[c]->[d]
zipWith3 z (a:as) (b:bs) (c:cs)
                          = z a b c : zipWith3 z as bs cs
zipWith3 _ _ _ _          = []
{-
unzip                    :: [(a,b)] -> ([a],[b])
unzip                     = foldr (\(a,b) ~(as,bs) -> (a:as, b:bs)) ([], [])

unzip3                   :: [(a,b,c)] -> ([a],[b],[c])
unzip3                    = foldr (\(a,b,c) ~(as,bs,cs) -> (a:as,b:bs,c:cs))
                                  ([],[],[])
-}
reverse   :: [a] -> [a]
reverse    = foldl (flip (:)) []

and, or   :: [Bool] -> Bool
and        = foldr (&&) True
or         = foldr (||) False

any, all  :: (a -> Bool) -> [a] -> Bool
any p      = or  . map p
all p      = and . map p

elem, notElem    :: Int -> [Int] -> Bool
elem              = any . (==)
notElem           = all . (/=)
{-
lookup           :: a -> [(a,b)] -> Maybe b
lookup k []       = Nothing
lookup k ((x,y):xys)
      | k==x      = Just y
      | otherwise = lookup k xys
-}
sum, product     :: [Int] -> Int
sum               = foldl (+) 0
product           = foldl (*) 1

maximum, minimum :: [Int] -> Int
maximum           = foldl1 max
minimum           = foldl1 min

{------------------------------------------------------------------------------
-- Strings
-------------------------------------------------------------------------------}
data PackedString

foreign import ccall "primCStringToString" packedStringToString :: PackedString -> [Char]

{------------------------------------------------------------------------------
-- Error Handling
------------------------------------------------------------------------------}
error :: [Char] -> a
error s = undefined

undefined :: forall a . a
undefined = error "undefined"

{------------------------------------------------------------------------------
-- Enum (Specialised for Int)
------------------------------------------------------------------------------}
enumFrom               :: Int -> [Int]
enumFrom x             = [ x ..]

enumFromTo             :: Int -> Int -> [Int]
enumFromTo m n | m > n = []
               | otherwise  
                       = m : enumFromTo (m + 1) n

enumFromThen           :: Int -> Int -> [Int]
enumFromThen x y       = [ x, y ..]

enumFromThenTo         :: Int -> Int -> Int -> [Int]
enumFromThenTo x y z   = [ x, y .. z ]

{------------------------------------------------------------------------------
-- Num (Specialised for Int)
------------------------------------------------------------------------------}
negate          :: Int -> Int
negate x        = 0 - x

{------------------------------------------------------------------------------
-- Ord (Specialised for Int)
------------------------------------------------------------------------------}
max       :: Int -> Int -> Int
max x y   | x <= y      = y
          | otherwise   = x
min       :: Int -> Int -> Int
min x y   | x <= y      = x
          | otherwise   = y

{------------------------------------------------------------------------------
-- Some standard functions
-------------------------------------------------------------------------------}
fst            :: forall b . (a,b) -> a
fst (x,_)       = x

snd            :: forall a . (a,b) -> b
snd (_,y)       = y

curry          :: ((a,b) -> c) -> (a -> b -> c)
curry f x y     = f (x,y)

uncurry        :: (a -> b -> c) -> ((a,b) -> c)
uncurry f p     = f (fst p) (snd p)

id             :: a -> a
id    x         = x

const          :: a -> b -> a
const k _       = k

(.)            :: (b -> c) -> (a -> b) -> (a -> c)
(f . g) x       = f (g x)

flip           :: (a -> b -> c) -> b -> a -> c
flip f x y      = f y x

($)            :: (a -> b) -> a -> b
f $ x           = f x

