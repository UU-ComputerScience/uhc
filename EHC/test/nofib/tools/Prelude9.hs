-- This prelude can be compiled by EHC 9, and contains:
-- from Prelude8.hs:
-- * datatypes: Bool, Ordering, [], PackedString, Maybe, Either
-- * very polymorphic functions:  9id, flip etc.)
-- * functions on lists: (head, ++, filter, foldr etc.)
-- * primitives for Int
-- additional:
-- * classes Eq, Ord, Integral, Num, Bounded, Enum, Functor, Monad
-- *  Int   instance of Eq, Ord, Integral, Num, Bounded, Enum
--    List  instance of Eq, Ord, Functor, Monad
--    Maybe instance of Functor, Monad
-- * some overloaded functions for Eq, Num, Ord, Monad

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

-- Eq class ---------------------------------------------

class Eq a where
    (==), (/=) :: a -> a -> Bool
    -- default definitions
    -- Minimal complete definition: (==) or (/=)
    x == y      = not (x/=y)
    x /= y      = not (x==y)

-- Ord class ---------------------------------------------

class (Eq a) => Ord a where
    compare                :: a -> a -> Ordering
    (<), (<=), (>=), (>)   :: a -> a -> Bool
    max, min               :: a -> a -> a
    -- default definitions
    x >= y  =  x>y || x==y
    x <= y  =  x<y || x==y
    max x y   | x <= y      = y
              | otherwise   = x
    min x y   | x <= y      = x
              | otherwise   = y

-- Integral class-----------------------------------------

class Integral a where
   quot, rem, div, mod :: a -> a -> a


-- Num class----------------------------------------------

class (Eq a) => Num a where
    (+), (-), (*)  :: a -> a -> a
    negate         :: a -> a
    abs, signum    :: a -> a
    fromInt        :: Int -> a

-- Bounded class----------------------------------------------

class Bounded a where
    minBound, maxBound :: a

-- Enum class --------------------------------------------

class Enum a where
    succ, pred           :: a -> a
    toEnum               :: Int -> a
    fromEnum             :: a -> Int
    enumFrom             :: a -> [a]              -- [n..]
    enumFromThen         :: a -> a -> [a]         -- [n,m..]
    enumFromTo           :: a -> a -> [a]         -- [n..m]
    enumFromThenTo       :: a -> a -> a -> [a]    -- [n,n'..m]

    -- Minimal complete definition: toEnum, fromEnum
    succ                  = toEnum . (1+)       . fromEnum
    pred                  = toEnum . subtract 1 . fromEnum
    enumFrom x            = map toEnum [ fromEnum x ..]
    enumFromTo x y        = map toEnum [ fromEnum x .. fromEnum y ]
    enumFromThen x y      = map toEnum [ fromEnum x, fromEnum y ..]
    enumFromThenTo x y z  = map toEnum [ fromEnum x, fromEnum y .. fromEnum z ]

-- Functor class ------------------------------------------------------------

class Functor f where
    fmap :: (a -> b) -> (f a -> f b)

-- Monad class ------------------------------------------------------------

class Monad m where
    return :: a -> m a
    (>>=)  :: m a -> (a -> m b) -> m b
    (>>)   :: m a -> m b -> m b
    fail   :: [Char] -> m a

    -- Minimal complete definition: (>>=), return
    p >> q  = p >>= \ _ -> q
    fail s  = error s


--==========================INSTANCES==========================

-- Int instance of Eq, Ord, Integral, Num, Bounded, Enum

instance Eq  Int where 
  (==)    = primEqInt

instance Ord Int where
  compare = primCmpInt
  (<)     = primLtInt
  (>)     = primGtInt

instance Integral Int where
  quot    = primQuotInt
  rem     = primRemInt
  div     = primDivInt
  mod     = primModInt

instance Num Int where
  (+)           = primAddInt
  (-)           = primSubInt
  (*)           = primMulInt
  negate        = primNegInt
  fromInt x     = x
  abs x        | x >= 0    = x
               | otherwise = negate x
  signum x     | x == 0    =  0
               | x >  0    =  1
               | otherwise =  -1

instance Bounded Int where
  minBound = -32768
  maxBound = 32767

instance Enum Int where
  toEnum   x = x
  fromEnum x = x

  enumFrom x             =  x : enumFrom (x+1)

  enumFromTo x y 
    | x > y = []
    | otherwise          = x : enumFromTo (x+1) y

  enumFromThen x y       = x : enumFromThen y (y+(y-x))

  enumFromThenTo x y z 
    | x > z = []
    | otherwise          = x : enumFromThenTo y (y+(y-x)) z

-- List instance of Eq, Ord, Functor, Monad

instance Eq a => Eq [a] where
    []     == []     =  True
    (x:xs) == (y:ys) =  x==y && xs==ys
    _      == _      =  False

instance Ord a => Ord [a] where
    compare []     (_:_)  = LT
    compare []     []     = EQ
    compare (_:_)  []     = GT
    compare (x:xs) (y:ys) = compAux x y (compare xs ys)

compAux      :: Ord a => a -> a -> Ordering -> Ordering
compAux x y o = case compare x y of EQ -> o; LT -> LT; GT -> GT

instance Functor [] where
    fmap = map

instance Monad [] where
    (x:xs) >>= f = f x ++ (xs >>= f)
    []     >>= f = []
    return x     = [x]
    fail s       = []

-- Maybe instance of Functor, Monad

instance Functor Maybe where
    fmap f Nothing  = Nothing
    fmap f (Just x) = Just (f x)

instance Monad Maybe where
    Just x  >>= k = k x
    Nothing >>= k = Nothing
    return        = Just
    fail s        = Nothing

--==========================OVERLOADED FUNCTIONS=======================

-- even, odd:: Integral a => a -> Bool
even n           =  n `rem` 2 == 0
odd              =  not . even

-- gcd :: Integral a => a -> a -> a
gcd x y         = gcd' (abs x) (abs y)
                  where gcd' x y | y==0      = x
                                 | otherwise = gcd' y (x `rem` y)


elem, notElem    :: Eq a => a -> [a] -> Bool
elem              = any . (==)
notElem           = all . (/=)

lookup           :: Eq a => a -> [(a,b)] -> Maybe b
lookup k []       = Nothing
lookup k ((x,y):xys)
      | k==x      = Just y
      | otherwise = lookup k xys

subtract       :: Num a => a -> a -> a
subtract        = flip (-)

sum, product :: Num a => [a] -> a
sum               = foldl (+) (fromInt 0)
product           = foldl (*) (fromInt 1)

maximum, minimum :: Ord a => [a] -> a
maximum           = foldl1 max
minimum           = foldl1 min

sequence         :: Monad m => [m a] -> m [a]
sequence []       = return []
sequence (c:cs)   = do x  <- c
                       xs <- sequence cs
                       return (x:xs)

sequence_        :: Monad m => [m a] -> m ()
sequence_         = foldr (>>) (return ())

mapM             :: Monad m => (a -> m b) -> [a] -> m [b]
mapM f            = sequence . map f

mapM_            :: Monad m => (a -> m b) -> [a] -> m ()
mapM_ f           = sequence_ . map f

(=<<)            :: Monad m => (a -> m b) -> m a -> m b
f =<< x           = x >>= f
