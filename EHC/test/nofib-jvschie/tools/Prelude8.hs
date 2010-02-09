-- This prelude can be compiled by EHC 8, and contains:
-- * datatypes: Bool, Ordering, [], PackedString, Maybe, Either
-- * very polymorphic functions:  9id, flip etc.)
-- * functions on lists: (head, ++, filter, foldr etc.)
-- * primitives for Int
-- and for this prelude only (will be removed in Prelude9.hs)
-- * classes Eq, Ord, Integral, Num, Enum specialized to Int
-- * some overloaded functions specialized to Int

infixr 9  .
infixl 9  !!
infixr 8  ^, ^^, **
infixl 7  *, /, `quot`, `rem`, `div`, `mod`, :%, %
infixl 6  +, -
infixr 5  :
infixr 5  ++
infix  4  ==, /=, <, <=, >=, >, `elem`, `notElem`
infixr 3  &&
infixr 2  ||
infixl 1  >>, >>=
infixr 1  =<<
infixr 0  $, $!, `seq`


-- Boolean type -------------------------------------------------------------

data Bool    = False | True

(&&) :: Bool -> Bool -> Bool
False && x   = False
True  && x   = x

(||) :: Bool -> Bool -> Bool
False || x   = x
True  || x   = True

not         :: Bool -> Bool
not True     = False
not False    = True

otherwise   :: Bool
otherwise    = True

-- Ordering type ------------------------------------------------------------

data Ordering = LT | EQ | GT

-- Lists --------------------------------------------------------------------

data [] a = ''[]'' | a : [a]

-- Evaluation and strictness ------------------------------------------------

seq :: a -> b -> b -- forall a . a -> forall b . b -> b
x `seq` y = letstrict x' = x in y

f $! x                = x `seq` f x

asTypeOf       :: a -> a -> a
asTypeOf        = const

error :: [Char] -> a
error s = undefined

undefined :: forall a . a
undefined = error "undefined"


-- Some standard functions --------------------------------------------------

fst            :: (a,b) -> a
fst (x,_)       = x

snd            :: (a,b) -> b
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

until          :: (a -> Bool) -> (a -> a) -> a -> a
until p f x     = if p x then x else until p f (f x)

length           :: [a] -> Int
length            = foldl' (\n _ -> n + 1) 0

(!!)             :: [a] -> Int -> a
--xs     !! n | n<0 = error "Prelude.!!: negative index"
--[]     !! _       = error "Prelude.!!: index too large"
--(x:_)  !! 0       = x
(x:xs) !! n       = if n == 0
                    then x
                    else xs !! (n-1)

foldl'           :: (a -> b -> a) -> a -> [b] -> a
foldl' f a []     = a
foldl' f a (x:xs) = (foldl' f $! f a x) xs

cycle            :: [a] -> [a]
cycle []          = error "Prelude.cycle: empty list"
cycle xs          = xs' where xs'=xs++xs'


-- Standard list functions {PreludeList} ------------------------------------

head             :: [a] -> a
head (x:_)        = x

last             :: [a] -> a
last [x]          = x
last (_:xs)       = last xs

tail             :: [a] -> [a]
tail (_:xs)       = xs

init             :: [a] -> [a]
init [x]          = []
init (x:xs)       = x : init xs

null             :: [a] -> Bool
null []           = True
null (_:_)        = False

(++)             :: [a] -> [a] -> [a]
[]     ++ ys      = ys
(x:xs) ++ ys      = x : (xs ++ ys)

map              :: (a -> b) -> [a] -> [b]
map f xs          = [ f x | x <- xs ]

filter           :: (a -> Bool) -> [a] -> [a]
filter p xs       = [ x | x <- xs, p x ]

concat           :: [[a]] -> [a]
concat            = foldr (++) []

concatMap        :: (a -> [b]) -> [a] -> [b]
--concatMap f       = concat . map f
concatMap f []    = []
concatMap f (x:xs) = f x ++ concatMap f xs

foldl            :: (a -> b -> a) -> a -> [b] -> a
foldl f z []      = z
foldl f z (x:xs)  = foldl f (f z x) xs

foldl1           :: (a -> a -> a) -> [a] -> a
foldl1 f (x:xs)   = foldl f x xs

scanl            :: (a -> b -> a) -> a -> [b] -> [a]
scanl f q xs      = q : (case xs of
                         []   -> []
                         x:xs -> scanl f (f q x) xs)

scanl1           :: (a -> a -> a) -> [a] -> [a]
scanl1 _ []       = []
scanl1 f (x:xs)   = scanl f x xs

foldr            :: (a -> b -> b) -> b -> [a] -> b
foldr f z []      = z
foldr f z (x:xs)  = f x (foldr f z xs)

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

iterate          :: (a -> a) -> a -> [a]
iterate f x       = x : iterate f (f x)

repeat           :: a -> [a]
repeat x          = xs where xs = x:xs

replicate        :: Int -> a -> [a]
replicate n x     = take n (repeat x)

take                :: Int -> [a] -> [a]
take n _  | n <= 0  = []
take _ []           = []
take n (x:xs)       = x : take (n-1) xs

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

takeWhile1 :: (a -> Bool) -> [a] -> [a]
takeWhile1 p (x:xs) = x : if p x then takeWhile1 p xs else []

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
break p              = span (not . p)

zip              :: [a] -> [b] -> [(a,b)]
zip               = zipWith  (\a b -> (a,b))

zip3             :: [a] -> [b] -> [c] -> [(a,b,c)]
zip3              = zipWith3 (\a b c -> (a,b,c))

zipWith                  :: (a->b->c) -> [a]->[b]->[c]
zipWith z (a:as) (b:bs)   = z a b : zipWith z as bs
zipWith _ _      _        = []

zipWith3                 :: (a->b->c->d) -> [a]->[b]->[c]->[d]
zipWith3 z (a:as) (b:bs) (c:cs)
                          = z a b c : zipWith3 z as bs cs
zipWith3 _ _ _ _          = []

unzip                    :: [(a,b)] -> ([a],[b])
unzip                     = foldr (\(a,b) ~(as,bs) -> (a:as, b:bs)) ([], [])

unzip3                   :: [(a,b,c)] -> ([a],[b],[c])
unzip3                    = foldr (\(a,b,c) ~(as,bs,cs) -> (a:as,b:bs,c:cs))
                                  ([],[],[])

reverse   :: [a] -> [a]
reverse    = foldl (flip (:)) []

and, or   :: [Bool] -> Bool
and        = foldr (&&) True
or         = foldr (||) False

any, all  :: (a -> Bool) -> [a] -> Bool
any p      = or  . map p
all p      = and . map p


-- Maybe type ---------------------------------------------------------------

data Maybe a = Nothing | Just a

maybe             :: b -> (a -> b) -> Maybe a -> b
maybe n f Nothing  = n
maybe n f (Just x) = f x

-- Either type --------------------------------------------------------------

data Either a b = Left a | Right b

either              :: (a -> c) -> (b -> c) -> Either a b -> c
either l r (Left x)  = l x
either l r (Right y) = r y


--==========================PRIMITIVES==========================================
-- Primitive Int functions --------------------------------------------------

foreign import ccall primGtInt    :: Int -> Int -> Bool
foreign import ccall primLtInt    :: Int -> Int -> Bool
foreign import ccall primEqInt    :: Int -> Int -> Bool
foreign import ccall primCmpInt   :: Int -> Int -> Ordering

foreign import ccall primAddInt   :: Int -> Int -> Int
foreign import ccall primSubInt   :: Int -> Int -> Int
foreign import ccall primMulInt   :: Int -> Int -> Int
foreign import ccall primNegInt   :: Int -> Int
foreign import ccall primDivInt   :: Int -> Int -> Int
foreign import ccall primModInt   :: Int -> Int -> Int
foreign import ccall primQuotInt  :: Int -> Int -> Int
foreign import ccall primRemInt   :: Int -> Int -> Int

-- PackedString -------------------------------------------------------------

data PackedString
--foreign import ccall "primCStringToString" packedStringToString :: PackedString -> [Char]
foreign import ccall "primPackedStringNull" packedStringNull :: PackedString -> Bool
foreign import ccall "primPackedStringHead" packedStringHead :: PackedString -> Char
foreign import ccall "primPackedStringTail" packedStringTail :: PackedString -> PackedString

packedStringToString :: PackedString -> [Char]
packedStringToString p = if packedStringNull p 
                          then []
                          else packedStringHead p : packedStringToString (packedStringTail p)


--==========================CLASSES============================================
-- Eq (Specialised for Int)

x /= y  =  not (x == y)

(==)  =  primEqInt

-- Ord (Specialised for Int)

x >= y  =  x>y || x==y
x <= y  =  x<y || x==y

max x y   | x <= y      = y
          | otherwise   = x

min x y   | x <= y      = x
          | otherwise   = y

(>)   =  primGtInt
(<)   =  primLtInt

-- Integral (Specialised for Int)

quot = primQuotInt
rem  = primRemInt
div  = primDivInt
mod  = primModInt

-- Num (Specialised for Int)

negate          :: Int -> Int
negate x        = 0 - x

(+)   =  primAddInt
(-)   =  primSubInt
(*)   =  primMulInt

-- Enum (Specialised for Int)

enumFrom               :: Int -> [Int]
enumFrom x             =  x : enumFrom (x+1)

enumFromTo             :: Int -> Int -> [Int]
enumFromTo x y 
  | x > y = []
  | otherwise          = x : enumFromTo (x+1) y

enumFromThen           :: Int -> Int -> [Int]
enumFromThen x y       = x : enumFromThen y (y+(y-x))

enumFromThenTo         :: Int -> Int -> Int -> [Int]
enumFromThenTo x y z 
  | x > z = []
  | otherwise          = x : enumFromThenTo y (y+(y-x)) z

--===========OVERLOADED FUNCTIONS (implicitly instantiated to Int here)

even n           =  n `rem` 2 == 0
odd              =  not . even

gcd x y         = gcd' (abs x) (abs y)
                  where gcd' x y | y==0      = x
                                 | otherwise = gcd' y (x `rem` y)

abs x        | x >= 0    = x
             | otherwise = -x

signum x     | x == 0    =  0
             | x > 0     =  1
             | otherwise = -1

elem              = any . (==)
notElem           = all . (/=)

lookup k []       = Nothing
lookup k ((x,y):xys)
      | k==x      = Just y
      | otherwise = lookup k xys

sum               = foldl (+) 0
product           = foldl (*) 1

maximum           = foldl1 max
minimum           = foldl1 min
