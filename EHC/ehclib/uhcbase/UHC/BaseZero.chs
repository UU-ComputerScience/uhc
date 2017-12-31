%%[99
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}

module UHC.BaseZero   -- adapted from the Hugs prelude
(
    Eq         (..),
    Ord        (..),
    Bounded    (..),
    Functor    (..),

    ''[]''     (..),
    Bool       (..),
    Maybe      (..),
    Either     (..),
    Ordering   (..),
    Char, 
    Int, 
    String,
    Integer, 
    IO,

    IO            (..), 
    State         (..),
    IOWorld, ioWorld,
    RealWorld, realWorld,
    
        -- IO Exception
    SomeException'(..),
    ArithException(..),
    ArrayException(..),
    AsyncException(..),
    IOException       ,
    ExitCode      (..),
#if defined (__UHC_TARGET_C__) || defined (__UHC_TARGET_LLVM__)
    forceString,
#else
    throw,
#endif

    asTypeOf, error, undefined, seq, ($!),
 
 
    (&&), (||), not, otherwise,
    isSpace, isUpper, isLower, isAlpha, isDigit, 
    isAlphaNum, 
    maybe, 
    either,
 

    fst, snd, curry, uncurry, id, const, (.), flip, ($), until,
    map, (++), concat, filter,
    head, last, tail, init, null, 
    foldl, foldl1, scanl, scanl1, foldr, foldr1, scanr, scanr1,
    iterate, 
    repeat, 
    cycle,
    takeWhile, dropWhile, span, break,
    lines, 
    words, unlines, 
    unwords, 
    reverse, and, or,
    any, all, 
    elem, notElem, lookup,
    maximum, minimum, concatMap, 
    zip, zip3, zipWith, zipWith3, unzip, unzip3,


    ioFromPrim,

    unsafeCoerce,

    PackedString,
    packedStringToString, packedStringToInteger, 
    primGtInt, primEqChar,
    ByteArray,

    
    exitWithIntCode,

    ExplicitStackTrace,
    ImplicitStackTrace,
    pushExplicitStackTrace,

    V1, U1(..), Par1(..), Rec1(..), K1(..), M1(..)
  , (:+:)(..), (:*:)(..), (:.:)(..)

  , Rec0(..), Par0(..), R, P
  , D1(..), C1(..), S1(..), D, C, S

  , Datatype(..), Constructor(..), Selector(..)
  , Arity(..), Fixity(..), Associativity(..)
  , NoSelector

  , Representable0(..), Representable1(..)

  , Generic

) where


#include "IntLikeInstance.h"


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


asTypeOf       :: a -> a -> a
asTypeOf        = const

seq :: forall a b . a -> b -> b
x `seq` y = let !x' = x in y

f $! x                = x `seq` f x

foreign import prim "primUnsafeId" unsafeCoerce :: forall a b . a -> b



#if defined (__UHC_TARGET_C__) || defined (__UHC_TARGET_LLVM__)

forceString :: String -> String
forceString s = stringSum s `seq` s

stringSum :: String -> Int
stringSum [] = 0
stringSum (x:xs) = primCharToInt x + stringSum xs

foreign import prim primError :: String -> a

error          :: forall a . String -> a
error s         = primError (forceString s)

#else

error          :: forall a . String -> a
error s         = throw (ErrorCall s)

#endif

undefined :: forall a . a
undefined       = error "Prelude.undefined"


#if defined (__UHC_TARGET_C__) || defined (__UHC_TARGET_LLVM__)
#else

foreign import prim primThrowException :: forall a x . SomeException' x -> a

throw :: SomeException' x -> a
throw e = primThrowException e

#endif


data PackedString
  deriving Generic

foreign import prim "primPackedStringNull" packedStringNull :: PackedString -> Bool
foreign import prim "primPackedStringHead" packedStringHead :: PackedString -> Char
foreign import prim "primPackedStringTail" packedStringTail :: PackedString -> PackedString

packedStringToString :: PackedString -> [Char]
packedStringToString p = if packedStringNull p 
                          then []
                          else packedStringHead p : packedStringToString (packedStringTail p)



data ByteArray
  deriving Generic

foreign import prim primByteArrayLength   :: ByteArray -> Int
#if defined(__UHC_TARGET_JS__)
foreign import prim primByteArrayToPackedString :: ByteArray -> PackedString
primByteArrayToString = packedStringToString . primByteArrayToPackedString
#else
foreign import prim primByteArrayToString :: ByteArray -> String
#endif


#if defined (__UHC_TARGET_C__) || defined (__UHC_TARGET_LLVM__)
foreign import prim packedStringToInteger :: PackedString -> Integer
#elif defined(__UHC_TARGET_JS__) || defined(__UHC_TARGET_CR__)
foreign import prim "primPackedStringToInteger" packedStringToInteger :: PackedString -> Integer
#else
foreign import prim "primCStringToInteger" packedStringToInteger :: PackedString -> Integer
#endif



class Eq a where
    (==), (/=) :: a -> a -> Bool

    -- Minimal complete definition: (==) or (/=)
    x == y      = not (x/=y)
    x /= y      = not (x==y)

class (Eq a) => Ord a where
    compare                :: a -> a -> Ordering
    (<), (<=), (>=), (>)   :: a -> a -> Bool
    max, min               :: a -> a -> a

    -- Minimal complete definition: (<=) or compare
    -- using compare can be more efficient for complex types
    compare x y | x==y      = EQ
                | x<=y      = LT
                | otherwise = GT

    x <= y                  = compare x y /= GT
    x <  y                  = compare x y == LT
    x >= y                  = compare x y /= LT
    x >  y                  = compare x y == GT

    max x y   | x <= y      = y
              | otherwise   = x
    min x y   | x <= y      = x
              | otherwise   = y

class Bounded a where
    minBound, maxBound :: a

class Functor f where
    fmap :: (a -> b) -> (f a -> f b)

data Bool    = False | True
            deriving (Eq, Ord, Generic)

(&&), (||)  :: Bool -> Bool -> Bool
False && x   = False
True  && x   = x
False || x   = x
True  || x   = True

not         :: Bool -> Bool
not True     = False
not False    = True

otherwise   :: Bool
otherwise    = True

instance Bounded Bool where
    minBound = False
    maxBound = True


type String = [Char]    -- strings are lists of characters

#if defined(__UHC_TARGET_JAZY__) || defined(__UHC_TARGET_CR__)
foreign import prim   primEqChar    :: Char -> Char -> Bool
foreign import prim   primCmpChar   :: Char -> Char -> Ordering
foreign import prim   primCharToInt   :: Char -> Int
foreign import prim   primIntToChar   :: Int -> Char
#else
foreign import prim "primEqInt"   primEqChar    :: Char -> Char -> Bool
foreign import prim "primCmpInt"  primCmpChar   :: Char -> Char -> Ordering
foreign import prim "primUnsafeId"  primCharToInt   :: Char -> Int
foreign import prim "primUnsafeId"  primIntToChar   :: Int -> Char
#endif
foreign import prim "primCharIsUpper"   isUpper    :: Char -> Bool
foreign import prim "primCharIsLower"   isLower    :: Char -> Bool

instance Eq Char  where 
    (==)    = primEqChar

instance Ord Char where 
    compare = primCmpChar


isSpace :: Char -> Bool
isSpace c              =  c == ' '  ||
                          c == '\t' ||
                          c == '\n' ||
                          c == '\r' ||
                          c == '\f' ||
                          c == '\v' ||
                          c == '\xa0'

isDigit :: Char -> Bool
isDigit c              =  c >= '0'   &&  c <= '9'


isAlpha :: Char -> Bool
isAlpha c    = isUpper c || isLower c

isAlphaNum :: Char -> Bool
isAlphaNum c = isAlpha c || isDigit c

data Maybe a = Nothing | Just a
  -- deriving Generic
            -- deriving (Eq, Ord, Generic)
            
maybe             :: b -> (a -> b) -> Maybe a -> b
maybe n f Nothing  = n
maybe n f (Just x) = f x


data Either a b = Left a | Right b
  -- deriving Generic
            -- deriving (Eq, Ord, Generic)
                

either              :: (a -> c) -> (b -> c) -> Either a b -> c
either l r (Left x)  = l x
either l r (Right y) = r y


data Ordering = LT | EQ | GT
            deriving (Eq, Ord, Generic)
                


data [] a = ''[]'' | a : [a]
            -- deriving Generic

instance Ord a => Ord [a] where
    compare []     (_:_)  = LT
    compare []     []     = EQ
    compare (_:_)  []     = GT
    compare (x:xs) (y:ys) = primCompAux x y (compare xs ys)

primCompAux      :: Ord a => a -> a -> Ordering -> Ordering
primCompAux x y o = case compare x y of EQ -> o; LT -> LT; GT -> GT

PRIMS_BOUNDED(Int,primMinInt,primMaxInt)

PRIMS_EQ(Int,primEqInt,primNeInt)
PRIMS_ORD(Int,primCmpInt,primLtInt,primGtInt,primLeInt,primGeInt)

INSTANCE_EQ(Int,primEqInt,primNeInt)
INSTANCE_ORD(Int,primCmpInt,primLtInt,primGtInt,primLeInt,primGeInt)
INSTANCE_BOUNDED(Int,primMinInt,primMaxInt)

foreign import prim primEqInteger   :: Integer -> Integer -> Bool
foreign import prim primCmpInteger  :: Integer -> Integer -> Ordering

instance Eq  Integer where 
    (==)    = primEqInteger
    
instance Ord Integer where
    compare = primCmpInteger


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

until          :: (a -> Bool) -> (a -> a) -> a -> a
until p f x     = if p x then x else until p f (f x)


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


cycle            :: [a] -> [a]
cycle []          = error "Prelude.cycle: empty list"
cycle xs          = xs' where xs'=xs++xs'




takeWhile           :: (a -> Bool) -> [a] -> [a]
takeWhile p []       = []
takeWhile p (x:xs)
         | p x       = x : takeWhile p xs
         | otherwise = []

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

lines     :: String -> [String]
lines ""   = []
lines s    = let (l,s') = break ('\n'==) s
             in l : case s' of []      -> []
                               (_:s'') -> lines s''

words     :: String -> [String]
words s    = case dropWhile isSpace s of
                  "" -> []
                  s' -> w : words s''
                        where (w,s'') = break isSpace s'

unlines   :: [String] -> String
unlines []      = []
unlines (l:ls)  = l ++ '\n' : unlines ls

unwords   :: [String] -> String
unwords []      =  ""
unwords [w]     = w
unwords (w:ws)  = w ++ ' ' : unwords ws

reverse   :: [a] -> [a]
reverse    = foldl (flip (:)) []

and, or   :: [Bool] -> Bool
and        = foldr (&&) True
or         = foldr (||) False

any, all  :: (a -> Bool) -> [a] -> Bool
any p      = or  . map p
all p      = and . map p

elem, notElem    :: Eq a => a -> [a] -> Bool
elem              = any . (==)
notElem           = all . (/=)

lookup           :: Eq a => a -> [(a,b)] -> Maybe b
lookup k []       = Nothing
lookup k ((x,y):xys)
      | k==x      = Just y
      | otherwise = lookup k xys


maximum, minimum :: Ord a => [a] -> a
maximum           = foldl1 max
minimum           = foldl1 min

concatMap        :: (a -> [b]) -> [a] -> [b]
concatMap _ []      = []
concatMap f (x:xs)  = f x ++ concatMap f xs

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


data ExitCode = ExitSuccess | ExitFailure Int
            -- deriving (Eq, Ord, Generic)
                

data SomeException' x                          -- alphabetical order of constructors required, assumed Int encoding in comment
  = ArithException      ArithException      -- 0
  | ArrayException      ArrayException      -- 1
  | AssertionFailed     String              -- 2
  | AsyncException      AsyncException      -- 3
  | BlockedOnDeadMVar                       -- 4
  | Deadlock                                -- 5
  | ErrorCall           String              -- 6
  | ExitException       ExitCode            -- 7 
  | IOException         x                   -- 8 -- IO exceptions (from 'ioError')
  | NoMethodError       String              -- 9
  | NonTermination                          -- 10
  | PatternMatchFail    String              -- 11
  | RecConError         String              -- 12
  | RecSelError         String              -- 13
  | RecUpdError         String              -- 14
  -- deriving (Generic)

data ArithException
  = Overflow
  | Underflow
  | LossOfPrecision
  | DivideByZero
  | Denormal
  deriving (Eq, Ord, Generic)

data ArrayException
  = IndexOutOfBounds    String
  | UndefinedElement    String
  -- deriving (Eq, Ord, Generic)

data AsyncException
  = HeapOverflow							-- 0
  | StackOverflow		String				-- 1
  | ThreadKilled							-- 2
  -- deriving (Eq, Ord, Generic)



newtype State s = State s
data RealWorld = RealWorld			-- known to compiler
  deriving Generic
type IOWorld = State RealWorld

newtype IO a = IO {unIO :: (IOWorld -> (IOWorld, a))}


realWorld :: RealWorld
realWorld = RealWorld
ioWorld :: IOWorld
ioWorld = State realWorld


ioFromPrim :: (IOWorld -> a) -> IO a
ioFromPrim f
  = IO (\w -> let !x = f w
              in (w, x)
       )

foreign import prim primExitWith      :: forall a . Int -> a

exitWithIntCode     :: Int -> IO a
exitWithIntCode e   =  ioFromPrim (\_ -> primExitWith e)

class Generic a


#ifdef __UHC__
V1 :: * -> *
#endif
data V1 p

#ifdef __UHC__
U1 :: * -> *
#endif
data U1 p = U1

#ifdef __UHC__
Par1 :: * -> *
#endif
newtype Par1 p = Par1 { unPar1 :: p }


#ifdef __UHC__
Rec1 :: (* -> *) -> * -> *
#endif
newtype Rec1 f p = Rec1 { unRec1 :: f p }

#ifdef __UHC__
K1 :: * -> * -> * -> *
#endif
newtype K1 i c p = K1 { unK1 :: c }

#ifdef __UHC__
M1 :: * -> * -> (* -> *) -> * -> *
#endif
newtype M1 i c f p = M1 { unM1 :: f p }



infixr 5 :+:
#ifdef __UHC__
(:+:) :: (* -> *) -> (* -> *) -> * -> *
#endif
data (:+:) f g p = L1 { unL1 :: f p } | R1 { unR1 :: g p }

infixr 6 :*:
#ifdef __UHC__
(:*:) :: (* -> *) -> (* -> *) -> * -> *
#endif
data (:*:) f g p = f p :*: g p

infixr 7 :.:
#ifdef __UHC__
(:.:) :: (* -> *) -> (* -> *) -> * -> *
#endif
newtype (:.:) f g p = Comp1 (f (g p))

data R
data P

type Rec0  = K1 R
type Par0  = K1 P

data D
data C
data S

type D1 = M1 D

type C1 = M1 C

type S1 = M1 S

class Datatype d where
#ifdef __UHC__
  datatypeName :: t d f a -> String
  moduleName   :: t d f a -> String
#else
  datatypeName :: t d (f :: * -> *) a -> String
  moduleName   :: t d (f :: * -> *) a -> String
#endif


class Selector s where
  selName :: t s f a -> String


data NoSelector

instance Selector NoSelector where
  selName _ = ""

class Constructor c where
#ifdef __UHC__
  conName :: t c f a -> String
#else
  conName :: t c (f :: * -> *) a -> String
#endif

#ifdef __UHC__
  conFixity :: t c f a -> Fixity
#else
  conFixity :: t c (f :: * -> *) a -> Fixity
#endif  
  conFixity = const Prefix

#ifdef __UHC__
  conIsRecord :: t c f a -> Bool
#else
  conIsRecord :: t c (f :: * -> *) a -> Bool
#endif
  conIsRecord = const False

#ifdef __UHC__
  conIsTuple :: t c f a -> Arity
#else
  conIsTuple :: t c (f :: * -> *) a -> Arity
#endif
  conIsTuple = const NoArity

data Arity = NoArity | Arity Int
    -- deriving (Eq, Ord)
  

data Fixity = Prefix | Infix Associativity Int
    -- deriving (Eq, Ord)
  

data Associativity =  LeftAssociative 
                   |  RightAssociative
                   |  NotAssociative
    deriving (Eq, Ord)


class Representable0 a rep where
  from0  :: a -> rep x
  to0    :: rep x -> a

class Representable1 f rep where
  from1  :: f a -> rep a
  to1    :: rep a -> f a
%%]

%%[99


type ExplicitStackTrace = [String]

type ImplicitStackTrace = [(Int,String)]

pushExplicitStackTrace :: String -> ExplicitStackTrace -> ExplicitStackTrace
pushExplicitStackTrace = (:)

%%]

%%[99

class Bounded' f where
  minBound' :: f x
  maxBound' :: f x

instance Bounded' U1 where
  minBound' = U1
  maxBound' = U1

instance (Bounded' fT) => Bounded' (M1 iT cT fT) where
  minBound' = M1 minBound'
  maxBound' = M1 maxBound'

instance (Bounded' fT, Bounded' gT) => Bounded' (fT :*: gT) where
  minBound' = minBound' :*: minBound'
  maxBound' = maxBound' :*: maxBound'

instance (Bounded fT) => Bounded' (K1 iT fT) where
  minBound' = K1 minBound
  maxBound' = K1 maxBound

instance (Bounded' fT, Bounded' gT) => Bounded' (fT :+: gT) where
  minBound' = L1 minBound'
  maxBound' = R1 maxBound'

{-# DERIVABLE Bounded minBound minBoundDefault #-}
minBoundDefault  ::  (Representable0 aT repT, Bounded' repT)
                       =>  repT xT -> aT
minBoundDefault rep = to0 (minBound' `asTypeOf` rep)

{-# DERIVABLE Bounded maxBound maxBoundDefault #-}
maxBoundDefault  ::  (Representable0 aT repT, Bounded' repT)
                       =>  repT xT -> aT
maxBoundDefault rep = to0 (maxBound' `asTypeOf` rep)

%%]

%%[99

class Eq' f where
  geq' :: f a -> f a -> Bool

instance Eq' U1 where
  geq' _ _ = True

instance (Eq c) => Eq' (K1 i c) where
  geq' (K1 a) (K1 b) = a == b


instance (Eq' a) => Eq' (M1 i c a) where
  geq' (M1 a) (M1 b) = geq' a b

instance (Eq' a, Eq' b) => Eq' (a :+: b) where
  geq' (L1 a) (L1 b) = geq' a b
  geq' (R1 a) (R1 b) = geq' a b
  geq' _      _      = False

instance (Eq' a, Eq' b) => Eq' (a :*: b) where
  geq' (a1 :*: b1) (a2 :*: b2) = geq' a1 a2 && geq' b1 b2

{-# DERIVABLE Eq (==) geqdefault #-}
deriving instance (Eq a) => Eq [a]

geqdefault :: (Representable0 a rep0, Eq' rep0) => rep0 x -> a -> a -> Bool
geqdefault (rep :: r) x y = geq' (from0 x :: r) (from0 y :: r)
%%]

%%[99

class Functor' f where
  fmap' :: (a -> b) -> f a -> f b

instance Functor' U1 where
  fmap' f U1 = U1

instance Functor' Par1 where
  fmap' f (Par1 a) = Par1 (f a)

instance Functor' (K1 i c) where
  fmap' f (K1 a) = K1 a

instance (Functor f) => Functor' (Rec1 f) where
  fmap' f (Rec1 a) = Rec1 (fmap f a)

instance (Functor' f) => Functor' (M1 i c f) where
  fmap' f (M1 a) = M1 (fmap' f a)

instance (Functor' f, Functor' g) => Functor' (f :+: g) where
  fmap' f (L1 a) = L1 (fmap' f a)
  fmap' f (R1 a) = R1 (fmap' f a)

instance (Functor' f, Functor' g) => Functor' (f :*: g) where
  fmap' f (a :*: b) = fmap' f a :*: fmap' f b

instance (Functor f, Functor' g) => Functor' (f :.: g) where
  fmap' f (Comp1 x) = Comp1 (fmap (fmap' f) x)


fmapDefault :: (Representable1 f rep, Functor' rep)
            => rep a -> (a -> b) -> f a -> f b
fmapDefault ra f x = to1 (fmap' f (from1 x `asTypeOf` ra))

{-# DERIVABLE Functor fmap fmapDefault #-}

-- deriving instance Functor Maybe
-- deriving instance Functor []
%%]

