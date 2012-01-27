%%[99
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE CPP #-}


module UHC.Base   -- adapted from the Hugs prelude
(
-- Classes
    Eq         (..),
    Ord        (..),
    Bounded    (..),
    Num        (..),
    Real       (..),
    Integral   (..),
    Fractional (..),
    Floating   (..),
    RealFrac   (..),
    RealFloat  (..),
    Enum       (..),
    Functor    (..),
    Monad      (..),
    Show       (..),
    Read       (..),

-- Types
    ''[]''     (..),
    Bool       (..),
    Maybe      (..),
    Either     (..),
    Ordering   (..),
    Ratio      (..), (%), 
    Char, 
    Int, 
    String,
    Integer, 
    Float, 
    Double, 
    IO,
    ShowS,
    ReadS,
    Rational, 

    IO            (..), 
    -- IOResult      (..),
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

--  dangerous functions
    asTypeOf, error, undefined, seq, ($!),
 
 
-- functions on specific types    
    -- Bool
    (&&), (||), not, otherwise,
    -- Char
    isSpace, isUpper, isLower, isAlpha, isDigit, isOctDigit, isHexDigit, isAlphaNum, showLitChar, readLitChar, lexLitChar,
    -- Ratio
    numerator, denominator,
    -- Maybe
    maybe, 
    -- Either
    either,
 
-- overloaded functions
    mapM, mapM_, sequence, sequence_, (=<<),
    subtract, even, odd, gcd, lcm, (^), (^^), absReal, signumReal,
    fromIntegral, realToFrac,
    boundedSucc, boundedPred, boundedEnumFrom, boundedEnumFromTo, boundedEnumFromThen, boundedEnumFromThenTo,
    shows, showChar, showString, showParen,
    --reads, read, lex, readParen, readSigned, readInt, readDec, readOct, readHex, readSigned, readFloat, lexDigits, 
    reads, read, lex, readParen, readSigned, readInt, readDec, readOct, readHex, readSigned, readFloat, lexDigits, 
    fromRat,

--  standard functions
    fst, snd, curry, uncurry, id, const, (.), flip, ($), until,
    map, (++), concat, filter,
    head, last, tail, init, null, length, (!!),
    foldl, foldl1, scanl, scanl1, foldr, foldr1, scanr, scanr1,
    iterate, repeat, replicate, cycle,
    take, drop, splitAt, takeWhile, dropWhile, span, break,
    lines, words, unlines, unwords, reverse, and, or,
    any, all, elem, notElem, lookup,
    sum, product, maximum, minimum, concatMap, 
    zip, zip3, zipWith, zipWith3, unzip, unzip3,

--  standard functions for Char
    ord, chr,

-- IO functions
    ioFromPrim,

-- Unsafe
    unsafeCoerce,

-- EHC specific functions
    PackedString,
    packedStringToString, packedStringToInteger, primIntToInteger,
    primGtInt, primEqChar,
    ByteArray,

-- EHC primitives, only exported to UHC. modules, hidden outside Prelude
    -- primEqInt,
    
-- System
    exitWithIntCode,

-- StackTrace
    ExplicitStackTrace,
    ImplicitStackTrace,
    pushExplicitStackTrace,

-- Generics
  -- * Generic representation types
    V1, U1(..), Par1(..), Rec1(..), K1(..), M1(..)
  , (:+:)(..), (:*:)(..), (:.:)(..)

  -- ** Synonyms for convenience
  , Rec0(..), Par0(..), R, P
  , D1(..), C1(..), S1(..), D, C, S

  -- * Meta-information
  , Datatype(..), Constructor(..), Selector(..)
  , Arity(..), Fixity(..), Associativity(..)
  , NoSelector

  -- * Representable type classes
  , Representable0(..), Representable1(..)

) where

-- import UHC.Generics

#include "IntLikeInstance.h"

--------------------------------------------------------------
-- Operator precedences
--------------------------------------------------------------

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

--------------------------------------------------------------
-- Dangerous functions
--------------------------------------------------------------

asTypeOf       :: a -> a -> a
asTypeOf        = const

seq :: forall a b . a -> b -> b
x `seq` y = letstrict x' = x in y

f $! x                = x `seq` f x

foreign import prim "primUnsafeId" unsafeCoerce :: forall a b . a -> b


----------------------------------------------------------------
-- error, undefined
----------------------------------------------------------------

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

----------------------------------------------------------------
-- Throw exception
----------------------------------------------------------------

#if defined (__UHC_TARGET_C__) || defined (__UHC_TARGET_LLVM__)
-- defined in UHC.OldException, on top of error because exceptions are not implemented, and show of exc is needed.
#else

foreign import prim primThrowException :: forall a x . SomeException' x -> a

throw :: SomeException' x -> a
throw e = primThrowException e

#endif

----------------------------------------------------------------
-- PackedString
----------------------------------------------------------------

data PackedString

-- foreign import prim "primCStringToString"  packedStringToString  :: PackedString -> [Char]
foreign import prim "primPackedStringNull" packedStringNull :: PackedString -> Bool
foreign import prim "primPackedStringHead" packedStringHead :: PackedString -> Char
foreign import prim "primPackedStringTail" packedStringTail :: PackedString -> PackedString

packedStringToString :: PackedString -> [Char]
packedStringToString p = if packedStringNull p 
                          then []
                          else packedStringHead p : packedStringToString (packedStringTail p)


----------------------------------------------------------------
-- ByteArray
----------------------------------------------------------------

data ByteArray

foreign import prim primByteArrayLength   :: ByteArray -> Int
#if defined(__UHC_TARGET_JS__)
foreign import prim primByteArrayToPackedString :: ByteArray -> PackedString
primByteArrayToString = packedStringToString . primByteArrayToPackedString
#else
foreign import prim primByteArrayToString :: ByteArray -> String
#endif


#if defined (__UHC_TARGET_C__) || defined (__UHC_TARGET_LLVM__)
foreign import prim packedStringToInteger :: PackedString -> Integer
#elif defined(__UHC_TARGET_JS__)
foreign import prim "primPackedStringToInteger" packedStringToInteger :: PackedString -> Integer
#else
foreign import prim "primCStringToInteger" packedStringToInteger :: PackedString -> Integer
#endif


--------------------------------------------------------------
-- class Eq, Ord, Bounded
--------------------------------------------------------------

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
    -- Minimal complete definition: All

boundedSucc, boundedPred :: (Num a, Bounded a, Enum a) => a -> a
boundedSucc x
  | x == maxBound = error "succ: applied to maxBound"
  | otherwise     = x+1
boundedPred x
  | x == minBound = error "pred: applied to minBound"
  | otherwise     = x-1

boundedEnumFrom       :: (Ord a, Bounded a, Enum a) => a -> [a]
boundedEnumFromTo     :: (Ord a, Bounded a, Enum a) => a -> a -> [a]
boundedEnumFromThenTo :: (Ord a, Num a, Bounded a, Enum a) => a -> a -> a -> [a]
boundedEnumFromThen   :: (Ord a, Bounded a, Enum a) => a -> a -> [a]

boundedEnumFrom n     = takeWhile1 (/= maxBound) (iterate succ n)
boundedEnumFromTo n m = takeWhile (<= m) (boundedEnumFrom n)
boundedEnumFromThen n m =
    enumFromThenTo n m (if n <= m then maxBound else minBound)
boundedEnumFromThenTo n n' m
  | n' >= n   = if n <= m then takeWhile1 (<= m - delta) ns else []
  | otherwise = if n >= m then takeWhile1 (>= m - delta) ns else []
 where
  delta = n'-n
  ns = iterate (+delta) n

-- takeWhile and one more
takeWhile1 :: (a -> Bool) -> [a] -> [a]
takeWhile1 p (x:xs) = x : if p x then takeWhile1 p xs else []

numericEnumFrom        :: Num a => a -> [a]
numericEnumFromThen    :: Num a => a -> a -> [a]
numericEnumFromTo      :: (Ord a, Fractional a) => a -> a -> [a]
numericEnumFromThenTo  :: (Ord a, Fractional a) => a -> a -> a -> [a]
numericEnumFrom n            = iterate' (+1) n
numericEnumFromThen n m      = iterate' (+(m-n)) n
numericEnumFromTo n m        = takeWhile (<= m+1/2) (numericEnumFrom n)
numericEnumFromThenTo n n' m = takeWhile p (numericEnumFromThen n n')
                               where p | n' >= n   = (<= m + (n'-n)/2)
                                       | otherwise = (>= m + (n'-n)/2)

iterate' :: (a -> a) -> a -> [a]        -- strict version of iterate
#if defined (__UHC_TARGET_C__) || defined (__UHC_TARGET_LLVM__)
iterate' f x = x : (iterate' f $! f x)
#else
iterate' f x = x : (letstrict fx = f x in iterate' f fx)
#endif

--------------------------------------------------------------
-- Numeric classes: Num, Real, Integral, 
--                  Fractional, Floating, 
--                  RealFrac, RealFloat
--------------------------------------------------------------

-- class (Eq a, Show a) => Num a where
class (Eq a) => Num a where
    (+), (-), (*)  :: a -> a -> a
    negate         :: a -> a
    abs, signum    :: a -> a
    fromInteger    :: Integer -> a
    fromInt        :: Int -> a

    -- Minimal complete definition: All, except negate or (-)
    x - y           = x + negate y
    fromInt         = fromIntegral
    negate x        = 0 - x

class (Num a, Ord a) => Real a where
    toRational     :: a -> Rational

class (Real a, Enum a) => Integral a where
    quot, rem, div, mod :: a -> a -> a
    quotRem, divMod     :: a -> a -> (a,a)
    toInteger           :: a -> Integer
    toInt               :: a -> Int

    -- Minimal complete definition: quotRem and toInteger
    n `quot` d           = q where (q,r) = quotRem n d
    n `rem` d            = r where (q,r) = quotRem n d
    n `div` d            = q where (q,r) = divMod n d
    n `mod` d            = r where (q,r) = divMod n d
    divMod n d           = if signum r == - signum d then (q-1, r+d) else qr
                           where qr@(q,r) = quotRem n d
    toInt                = toInt . toInteger

class (Num a) => Fractional a where
    (/)          :: a -> a -> a
    recip        :: a -> a
    fromRational :: Rational -> a
    fromDouble   :: Double -> a

    -- Minimal complete definition: fromRational and ((/) or recip)
    recip x       = 1 / x
    fromDouble x  = fromRational (fromDouble x)
    x / y         = x * recip y


class (Fractional a) => Floating a where
    pi                  :: a
    exp, log, sqrt      :: a -> a
    (**), logBase       :: a -> a -> a
    sin, cos, tan       :: a -> a
    asin, acos, atan    :: a -> a
    sinh, cosh, tanh    :: a -> a
    asinh, acosh, atanh :: a -> a

    -- Minimal complete definition: pi, exp, log, sin, cos, sinh, cosh,
    --                              asinh, acosh, atanh
    pi                   = 4 * atan 1
    x ** y               = exp (log x * y)
    logBase x y          = log y / log x
    sqrt x               = x ** 0.5
    tan x                = sin x / cos x
    sinh x               = (exp x - exp (-x)) / 2
    cosh x               = (exp x + exp (-x)) / 2
    tanh x               = sinh x / cosh x
    asinh x              = log (x + sqrt (x*x + 1))
    acosh x              = log (x + sqrt (x*x - 1))
    atanh x              = (log (1 + x) - log (1 - x)) / 2

class (Real a, Fractional a) => RealFrac a where
    properFraction   :: (Integral b) => a -> (b,a)
    truncate, round  :: (Integral b) => a -> b
    ceiling, floor   :: (Integral b) => a -> b

    -- Minimal complete definition: properFraction
    truncate x        = m where (m,_) = properFraction x

    round x           = let (n,r) = properFraction x
                            m     = if r < 0 then n - 1 else n + 1
                        in case signum (abs r - 0.5) of
                            -1 -> n
                            0  -> if even n then n else m
                            1  -> m

    ceiling x         = if r > 0 then n + 1 else n
                        where (n,r) = properFraction x

    floor x           = if r < 0 then n - 1 else n
                        where (n,r) = properFraction x
{-----------------------------
    -- Minimal complete definition: properFraction
    truncate x :: xt  = m where (m::xt,_) = properFraction x

    round x :: xt     = let (n::xt,r) = properFraction x
                            m     = if r < 0 then n - 1 else n + 1
                        in case signum (abs r - 0.5) of
                            -1 -> n
                            0  -> if even n then n else m
                            1  -> m

    ceiling x :: xt   = if r > 0 then n + 1 else n
                        where (n::xt,r) = properFraction x

    floor x :: xt     = if r < 0 then n - 1 else n
                        where (n::xt,r) = properFraction x
-----------------------------}


class (RealFrac a, Floating a) => RealFloat a where
    floatRadix       :: a -> Integer
    floatDigits      :: a -> Int
    floatRange       :: a -> (Int,Int)
    decodeFloat      :: a -> (Integer,Int)
    encodeFloat      :: Integer -> Int -> a
    exponent         :: a -> Int
    significand      :: a -> a
    scaleFloat       :: Int -> a -> a
    isNaN, isInfinite, isDenormalized, isNegativeZero, isIEEE
                     :: a -> Bool
    atan2            :: a -> a -> a

    -- Minimal complete definition: All, except exponent, signficand,
    --                              scaleFloat, atan2
    exponent x        = if m==0 then 0 else n + floatDigits x
                        where (m,n) = decodeFloat x
    significand x     = encodeFloat m (- floatDigits x)
                        where (m,_) = decodeFloat x
    scaleFloat k x    = encodeFloat m (n+k)
                        where (m,n) = decodeFloat x
    atan2 y x
      | x>0           = atan (y/x)
      | x==0 && y>0   = pi/2
      | x<0 && y>0    = pi + atan (y/x)
      | (x<=0 && y<0) ||
        (x<0 && isNegativeZero y) ||
        (isNegativeZero x && isNegativeZero y)
                      = - atan2 (-y) x
      | y==0 && (x<0 || isNegativeZero x)
                      = pi    -- must be after the previous test on zero y
      | x==0 && y==0  = y     -- must be after the other double zero tests
      | otherwise     = x + y -- x or y is a NaN, return a NaN (via +)

--------------------------------------------------------------
-- Overloaded numeric functions
--------------------------------------------------------------

subtract       :: Num a => a -> a -> a
subtract        = flip (-)

even, odd        :: (Integral a) => a -> Bool
even n           =  n `rem` 2 == 0
odd              =  not . even

gcd            :: Integral a => a -> a -> a
gcd 0 0         = error "Prelude.gcd: gcd 0 0 is undefined"
gcd x y         = gcd' (abs x) (abs y)
                  where gcd' x 0 = x
                        gcd' x y = gcd' y (x `rem` y)

lcm            :: (Integral a) => a -> a -> a
lcm _ 0         = 0
lcm 0 _         = 0
lcm x y         = abs ((x `quot` gcd x y) * y)

(^)            :: (Num a, Integral b) => a -> b -> a
x ^ 0           = 1
x ^ n  | n > 0  = f x (n-1) x
                  where f _ 0 y = y
                        f x n y = g x n where
                                  g x n | even n    = g (x*x) (n`quot`2)
                                        | otherwise = f x (n-1) (x*y)
_ ^ _           = error "Prelude.^: negative exponent"

(^^)           :: (Fractional a, Integral b) => a -> b -> a
x ^^ n          = if n >= 0 then x ^ n else recip (x^(-n))

fromIntegral   :: (Integral a, Num b) => a -> b
fromIntegral    = fromInteger . toInteger

realToFrac     :: (Real a, Fractional b) => a -> b
realToFrac      = fromRational . toRational

absReal :: (Ord a,Num a) => a -> a
absReal x    | x >= 0    = x
             | otherwise = -x

signumReal :: (Ord a,Num a) => a -> a
signumReal x | x == 0    =  0
             | x > 0     =  1
             | otherwise = -1



--------------------------------------------------------------
-- class Enum
--------------------------------------------------------------

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

--------------------------------------------------------------
-- class Read, Show
--------------------------------------------------------------

type ReadS a = String -> [(a,String)]
type ShowS   = String -> String

class Read a where
    readsPrec :: Int -> ReadS a
    readList  :: ReadS [a]

    -- Minimal complete definition: readsPrec
    readList  :: ReadS [a]
               = readParen False (\r -> [pr | ("[",s) <- lex r,
                                              pr      <- readl s ])
                 where readl :: ReadS [a]
                       readl  s = [([],t)   | ("]",t) <- lex s] ++
                                  [(x:xs,u) | (x,t)   <- reads s,
                                              (xs,u)  <- readl' t]
                       readl' :: ReadS [a]
                       readl' s = [([],t)   | ("]",t) <- lex s] ++
                                  [(x:xs,v) | (",",t) <- lex s,
                                              (x,u)   <- reads t,
                                              (xs,v)  <- readl' u]

class Show a where
    show      :: a -> String
    showsPrec :: Int -> a -> ShowS
    showList  :: [a] -> ShowS

    -- Minimal complete definition: show or showsPrec
    show x          = showsPrec 0 x ""
    showsPrec _ x s = show x ++ s
    showList []     = showString "[]"
    showList (x:xs) = showChar '[' . shows x . showl xs
                      where showl []     = showChar ']'
                            showl (x:xs) = showChar ',' . shows x . showl xs

--------------------------------------------------------------
-- class Functor, Monad
--------------------------------------------------------------

class Functor f where
    fmap :: (a -> b) -> (f a -> f b)

class Monad m where
    return :: a -> m a
    (>>=)  :: m a -> (a -> m b) -> m b
    (>>)   :: m a -> m b -> m b
    fail   :: String -> m a

    -- Minimal complete definition: (>>=), return
    p >> q  = p >>= \ _ -> q
    fail s  = error s

sequence       :: Monad m => [m a] -> m [a]
sequence []     = return []
sequence (c:cs) = do x  <- c
                     xs <- sequence cs
                     return (x:xs)

-- overloaded Functor and Monad function

-- sequence_        :: forall a . Monad m => [m a] -> m ()
sequence_        :: Monad m => [m a] -> m ()
sequence_         = foldr (>>) (return ())

--mapM             :: Monad m => (a -> m b) -> [a] -> m [b]
mapM             :: forall a b . Monad m => (a -> m b) -> [a] -> m [b]
mapM f            = sequence . map f

--mapM_            :: Monad m => (a -> m b) -> [a] -> m ()
mapM_            :: forall a b . Monad m => (a -> m b) -> [a] -> m ()
mapM_ f           = sequence_ . map f

(=<<)            :: Monad m => (a -> m b) -> m a -> m b
f =<< x           = x >>= f



--------------------------------------------------------------
-- Unit type
--------------------------------------------------------------

-- data () = () deriving (Eq, Ord, Ix, Enum, Read, Show, Bounded)

--------------------------------------------------------------
-- Boolean type
--------------------------------------------------------------

data Bool    = False | True
               -- deriving (Eq, Ord, Ix, Enum, Read, Show, Bounded)
               deriving (Eq, Ord, Enum, Show, Read)

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

--------------------------------------------------------------
-- Char type
--------------------------------------------------------------

-- type Char builtin
type String = [Char]    -- strings are lists of characters

#ifdef __UHC_TARGET_JAZY__
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

instance Enum Char where
    toEnum           = primIntToChar
    fromEnum         = primCharToInt
    --enumFrom c       = map toEnum [fromEnum c .. fromEnum (maxBound::Char)]
    --enumFromThen     = boundedEnumFromThen

instance Read Char where
    readsPrec p      = readParen False
                            (\r -> [(c,t) | ('\'':s,t) <- lex r,
                                            (c,"\'")   <- readLitChar s ])
    readList = readParen False (\r -> [(l,t) | ('"':s, t) <- lex r,
                                               (l,_)      <- readl s ])
               where readl ('"':s)      = [("",s)]
                     readl ('\\':'&':s) = readl s
                     readl s            = [(c:cs,u) | (c ,t) <- readLitChar s,
                                                      (cs,u) <- readl t ]

instance Show Char where
    showsPrec p '\'' = showString "'\\''"
    showsPrec p c    = showChar '\'' . showLitChar c . showChar '\''

    showList cs   = showChar '"' . showl cs
                    where showl ""       = showChar '"'
                          showl ('"':cs) = showString "\\\"" . showl cs
                          showl (c:cs)   = showLitChar c . showl cs

instance Bounded Char where
    minBound = '\0'
    maxBound = '\xff' -- primMaxChar

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

ord :: Char -> Int
ord = fromEnum

chr :: Int -> Char
chr = toEnum

--------------------------------------------------------------
-- Maybe type
--------------------------------------------------------------

data Maybe a = Nothing | Just a
               deriving (Eq, Ord, Show, Read)  -- TODO: Read

{-
-- done via generic deriving
instance Functor Maybe where
    fmap f Nothing  = Nothing
    fmap f (Just x) = Just (f x)
-}

instance Monad Maybe where
    Just x  >>= k = k x
    Nothing >>= k = Nothing
    return        = Just
    fail s        = Nothing

maybe             :: b -> (a -> b) -> Maybe a -> b
maybe n f Nothing  = n
maybe n f (Just x) = f x

--------------------------------------------------------------
-- Either type
--------------------------------------------------------------

data Either a b = Left a | Right b
                  deriving (Eq, Ord, Show) -- TODO: Read

either              :: (a -> c) -> (b -> c) -> Either a b -> c
either l r (Left x)  = l x
either l r (Right y) = r y

--------------------------------------------------------------
-- Ordering type
--------------------------------------------------------------

data Ordering = LT | EQ | GT
                deriving (Eq, Ord, Enum, Show) -- TODO: Ix, Read, Bounded

--------------------------------------------------------------
-- Lists
--------------------------------------------------------------

data [] a = ''[]'' | a : [a]

{-
instance Eq a => Eq [a] where
    []     == []     =  True
    (x:xs) == (y:ys) =  x==y && xs==ys
    _      == _      =  False
    []     /= []     =  False
    (x:xs) /= (y:ys) =  x/=y || xs/=ys
    _      /= _      =  True
-}

instance Ord a => Ord [a] where
    compare []     (_:_)  = LT
    compare []     []     = EQ
    compare (_:_)  []     = GT
    compare (x:xs) (y:ys) = primCompAux x y (compare xs ys)

{-
-- done via generic deriving
instance Functor [] where
    fmap = map
-}

instance Monad [ ] where
    (x:xs) >>= f = f x ++ (xs >>= f)
    []     >>= f = []
    return x     = [x]
    fail s       = []

instance Read a => Read [a]  where
    readsPrec p = readList

instance Show a => Show [a]  where
    showsPrec p = showList

primCompAux      :: Ord a => a -> a -> Ordering -> Ordering
primCompAux x y o = case compare x y of EQ -> o; LT -> LT; GT -> GT


--------------------------------------------------------------
-- Int type
--------------------------------------------------------------
-- type Int builtin

PRIMS_BOUNDED(Int,primMinInt,primMaxInt)
PRIMS_CONVERSION_INTEGER(Int,primIntegerToInt,primIntToInteger)

PRIMS_EQ(Int,primEqInt,primNeInt)
PRIMS_ORD(Int,primCmpInt,primLtInt,primGtInt,primLeInt,primGeInt)
PRIMS_NUM(Int,primAddInt,primSubInt,primMulInt,primNegInt)

INSTANCE_EQ(Int,primEqInt,primNeInt)
INSTANCE_ORD(Int,primCmpInt,primLtInt,primGtInt,primLeInt,primGeInt)
INSTANCE_BOUNDED(Int,primMinInt,primMaxInt)
INSTANCE_REAL(Int)
INSTANCE_NUM(Int,primAddInt,primSubInt,primMulInt,primNegInt,primIntegerToInt,id)


foreign import prim primDivInt       :: Int -> Int -> Int
foreign import prim primModInt       :: Int -> Int -> Int
foreign import prim primQuotInt      :: Int -> Int -> Int
foreign import prim primRemInt       :: Int -> Int -> Int


#if defined (__UHC_TARGET_C__) || defined (__UHC_TARGET_LLVM__)

instance Integral Int where
    divMod x y   = (primDivInt x y, primModInt x y)
    quotRem x y  = (primQuotInt x y, primRemInt x y)
    div       = primDivInt
    quot      = primQuotInt
    rem       = primRemInt
    mod       = primModInt
    toInteger = primIntToInteger
    toInt x   = x

#else

foreign import prim primDivModInt    :: Int -> Int -> (Int,Int)
foreign import prim primQuotRemInt   :: Int -> Int -> (Int,Int)

instance Integral Int where
    divMod    = primDivModInt
    quotRem   = primQuotRemInt
    div       = primDivInt
    quot      = primQuotInt
    rem       = primRemInt
    mod       = primModInt
    toInteger = primIntToInteger
    toInt x   = x

#endif

instance Enum Int where
    succ           = boundedSucc
    pred           = boundedPred
    toEnum         = id
    fromEnum       = id
    enumFrom       = boundedEnumFrom
    enumFromTo     = boundedEnumFromTo
    enumFromThen   = boundedEnumFromThen
    enumFromThenTo = boundedEnumFromThenTo

instance Read Int where
    readsPrec p = readSigned readDec


#if defined (__UHC_TARGET_C__) || defined (__UHC_TARGET_LLVM__)
{-
 This implementation fails for showInt minBound because in 2's complement arithmetic
 -minBound == maxBound+1 == minBound
-}
showInt :: Int -> String
showInt x | x<0  = '-' : showInt(-x)
          | x==0 = "0"
          | otherwise = (map primIntToChar . map (+48) . reverse . map (`rem`10) . takeWhile (/=0) . iterate (`div`10)) x

instance Show Int where
    show   = showInt
    
#else

instance Show Int where
    show   = show . toInteger

#endif



--------------------------------------------------------------
-- Integer type
--------------------------------------------------------------

foreign import prim primEqInteger   :: Integer -> Integer -> Bool
foreign import prim primCmpInteger  :: Integer -> Integer -> Ordering

foreign import prim primDivInteger  :: Integer -> Integer -> Integer
foreign import prim primModInteger  :: Integer -> Integer -> Integer

#if defined(__UHC_TARGET_JS__)
-- Integers are represented by `jsbn' BigInteger library
{-
foreign import prim "primEqInt"   primEqInteger      :: Integer -> Integer -> Bool
foreign import prim "primCmpInt"  primCmpInteger     :: Integer -> Integer -> Ordering
foreign import prim "primAddInt"  primAddInteger     :: Integer -> Integer -> Integer
foreign import prim "primSubInt"  primSubInteger     :: Integer -> Integer -> Integer
foreign import prim "primMulInt"  primMulInteger     :: Integer -> Integer -> Integer
foreign import prim "primNegInt"  primNegInteger     :: Integer -> Integer
foreign import prim "primQuotInt" primQuotInteger    :: Integer -> Integer -> Integer
foreign import prim "primRemInt"  primRemInteger     :: Integer -> Integer -> Integer
foreign import prim "primDivInt"  primDivInteger     :: Integer -> Integer -> Integer
foreign import prim "primModInt"  primModInteger     :: Integer -> Integer -> Integer
foreign import prim "primQuotRemInt" primQuotRemInteger       :: Integer -> Integer -> (Integer,Integer)
foreign import prim "primDivModInt"  primDivModInteger        :: Integer -> Integer -> (Integer,Integer)
-}
foreign import js "%1.add(%*)"				primAddInteger 				:: Integer -> Integer -> Integer
foreign import js "%1.subtract(%*)" 		primSubInteger 				:: Integer -> Integer -> Integer
foreign import js "%1.multiply(%*)" 		primMulInteger 				:: Integer -> Integer -> Integer
foreign import js "%1.negate(%*)" 			primNegInteger 				:: Integer -> Integer
foreign import js "%1.divide(%*)" 			primQuotInteger 			:: Integer -> Integer -> Integer
foreign import js "%1.remainder(%*)" 		primRemInteger 				:: Integer -> Integer -> Integer
#else
foreign import prim primAddInteger  :: Integer -> Integer -> Integer
foreign import prim primSubInteger  :: Integer -> Integer -> Integer
foreign import prim primMulInteger  :: Integer -> Integer -> Integer
foreign import prim primNegInteger  :: Integer -> Integer
foreign import prim primQuotInteger :: Integer -> Integer -> Integer
foreign import prim primRemInteger  :: Integer -> Integer -> Integer
#endif

#if defined( __UHC_TARGET_BC__ ) || defined(__UHC_TARGET_JS__)
foreign import prim primQuotRemInteger       :: Integer -> Integer -> (Integer,Integer)
foreign import prim primDivModInteger        :: Integer -> Integer -> (Integer,Integer)
#endif

instance Eq  Integer where 
    (==)    = primEqInteger
    
instance Ord Integer where
    compare = primCmpInteger

instance Num Integer where
    (+)           = primAddInteger
    (-)           = primSubInteger
    negate        = primNegInteger
    (*)           = primMulInteger
    abs           = absReal
    signum        = signumReal
    fromInteger x = x
    fromInt       = primIntToInteger

instance Real Integer where
    toRational x = x % 1


#if defined( __UHC_TARGET_C__) || defined(__UHC_TARGET_JAZY__)  || defined (__UHC_TARGET_LLVM__)

instance Integral Integer where
    divMod x y  = (primDivInteger x y, primModInteger x y)
    quotRem x y = (primQuotInteger x y, primRemInteger x y)
    div         = primDivInteger
    quot        = primQuotInteger
    rem         = primRemInteger
    mod         = primModInteger
    toInteger x = x
    toInt       = primIntegerToInt

#else

instance Integral Integer where
    divMod      = primDivModInteger
    quotRem     = primQuotRemInteger
    div         = primDivInteger
    quot        = primQuotInteger
    rem         = primRemInteger
    mod         = primModInteger
    toInteger x = x
    toInt       = primIntegerToInt

#endif

instance Enum Integer where
    succ x         = x + 1
    pred x         = x - 1

    toEnum         = primIntToInteger
    fromEnum       = primIntegerToInt
    enumFrom       = numericEnumFrom
    enumFromThen   = numericEnumFromThen
    enumFromTo n m = takeWhile (<= m) (numericEnumFrom n)
    enumFromThenTo n n2 m = takeWhile p (numericEnumFromThen n n2)
                                where p | n2 >= n   = (<= m)
                                        | otherwise = (>= m)

#if defined (__UHC_TARGET_C__) || defined (__UHC_TARGET_LLVM__)
{-
 This implementation fails for showInt minBound because in 2's complement arithmetic
 -minBound == maxBound+1 == minBound
-}

showInteger :: Integer -> String
showInteger x | x<0  = '-' : showInteger(-x)
          | x==0 = "0"
          | otherwise = (map primIntToChar . map (+48) . reverse . map primIntegerToInt . map (`rem`10) . takeWhile (/=0) . iterate (`div`10)) x

instance Show Integer where
    show   = showInteger
    
#elif defined( __UHC_TARGET_JS__ )

foreign import js "%1.toString()" 			primIntegerToPackedString 	:: Integer -> PackedString

instance Show Integer where
    show   = packedStringToString . primIntegerToPackedString

#elif defined( __UHC_TARGET_JAZY__ )

foreign import prim primShowIntegerToPackedString :: Integer -> PackedString

instance Show Integer where
    show   = packedStringToString . primShowIntegerToPackedString

#else

foreign import prim primShowInteger :: Integer -> String

instance Show Integer where
    show   = primShowInteger

#endif


instance Read Integer where
    readsPrec p = readSigned readDec


--------------------------------------------------------------
-- Float and Double type
--------------------------------------------------------------

data Float     -- opaque datatype of 32bit IEEE floating point numbers
data Double    -- opaque datatype of 64bit IEEE floating point numbers

#if defined(__UHC_TARGET_JS__)
foreign import prim "primEqInt"          	primEqFloat             :: Float -> Float -> Bool
foreign import prim "primCmpInt"         	primCmpFloat            :: Float -> Float -> Ordering

foreign import prim "primEqInt"         	primEqDouble            :: Double -> Double -> Bool
foreign import prim "primCmpInt"        	primCmpDouble           :: Double -> Double -> Ordering

foreign import prim "primAddInt"         	primAddFloat            :: Float -> Float -> Float
foreign import prim "primSubInt"         	primSubFloat            :: Float -> Float -> Float
foreign import prim "primMulInt"         	primMulFloat            :: Float -> Float -> Float
foreign import prim "primNegInt"         	primNegFloat            :: Float -> Float
foreign import prim "primUnsafeId"       	primIntToFloat          :: Int -> Float

foreign import prim "primAddInt"        	primAddDouble           :: Double -> Double -> Double
foreign import prim "primSubInt"        	primSubDouble           :: Double -> Double -> Double
foreign import prim "primMulInt"        	primMulDouble           :: Double -> Double -> Double
foreign import prim "primNegInt"        	primNegDouble           :: Double -> Double
foreign import prim "primUnsafeId"      	primIntToDouble         :: Int -> Double

foreign import prim "primQuotInt"      		primDivideFloat         :: Float -> Float -> Float
foreign import prim "primRecipDouble"       primRecipFloat          :: Float -> Float
foreign import prim "primUnsafeId"    		primDoubleToFloat       :: Double -> Float

foreign import prim "primUnsafeId"    		primFloatToDouble       :: Float -> Double
foreign import prim "primRationalToDouble"  primRationalToFloat     :: Rational -> Float
foreign import prim "primRationalToDouble" 	primRationalToDouble    :: Rational -> Double
#else
foreign import prim primEqFloat             :: Float -> Float -> Bool
foreign import prim primCmpFloat            :: Float -> Float -> Ordering

foreign import prim primEqDouble            :: Double -> Double -> Bool
foreign import prim primCmpDouble           :: Double -> Double -> Ordering

foreign import prim primAddFloat            :: Float -> Float -> Float
foreign import prim primSubFloat            :: Float -> Float -> Float
foreign import prim primMulFloat            :: Float -> Float -> Float
foreign import prim primNegFloat            :: Float -> Float
foreign import prim primIntToFloat          :: Int -> Float

foreign import prim primAddDouble           :: Double -> Double -> Double
foreign import prim primSubDouble           :: Double -> Double -> Double
foreign import prim primMulDouble           :: Double -> Double -> Double
foreign import prim primNegDouble           :: Double -> Double
foreign import prim primIntToDouble         :: Int -> Double

foreign import prim primDivideFloat         :: Float -> Float -> Float
foreign import prim primRecipFloat          :: Float -> Float
foreign import prim primDoubleToFloat       :: Double -> Float

foreign import prim primFloatToDouble       :: Float -> Double
foreign import prim primRationalToFloat     :: Rational -> Float
foreign import prim primRationalToDouble    :: Rational -> Double
#endif

#if defined(__UHC_TARGET_JS__)
foreign import js "%1.doubleValue()"  		primIntegerToFloat      :: Integer -> Float
foreign import js "%1.doubleValue()"  		primIntegerToDouble     :: Integer -> Double
#else
foreign import prim primIntegerToFloat      :: Integer -> Float
foreign import prim primIntegerToDouble     :: Integer -> Double
#endif

instance Eq  Float  where (==) = primEqFloat
instance Eq  Double where (==) = primEqDouble

instance Ord Float  where compare = primCmpFloat
instance Ord Double where compare = primCmpDouble


instance Num Float where
    (+)           = primAddFloat
    (-)           = primSubFloat
    negate        = primNegFloat
    (*)           = primMulFloat
    abs           = absReal
    signum        = signumReal
    fromInteger   = primIntegerToFloat
    fromInt       = primIntToFloat


instance Num Double where
    (+)         = primAddDouble
    (-)         = primSubDouble
    negate      = primNegDouble
    (*)         = primMulDouble
    abs         = absReal
    signum      = signumReal
    fromInteger = primIntegerToDouble
    fromInt     = primIntToDouble

instance Real Float where
    toRational = floatToRational

instance Real Double where
    toRational = doubleToRational

-- TODO: Calls to these functions should be optimised when passed as arguments to fromRational
floatToRational  :: Float  -> Rational
doubleToRational :: Double -> Rational
floatToRational  x = fromRat x 
doubleToRational x = fromRat x

fromRat :: RealFloat a => a -> Rational
fromRat x = (m%1)*(b%1)^^n
          where (m,n) = decodeFloat x
                b     = floatRadix x


instance Fractional Float where
    (/)          = primDivideFloat
    recip        = primRecipFloat
    fromRational = primRationalToFloat
    fromDouble   = primDoubleToFloat

foreign import prim primDivideDouble :: Double -> Double -> Double
foreign import prim primRecipDouble :: Double -> Double

instance Fractional Double where
    (/)          = primDivideDouble
    recip        = primRecipDouble
    fromRational = primRationalToDouble
    fromDouble x = x

{-----------------------------
-- These primitives are equivalent to (and are defined using) 
-- rationalTo{Float,Double}.  The difference is that they test to see
-- if their argument is of the form (fromDouble x) - which allows a much
-- more efficient implementation.
primitive primRationalToFloat  :: Rational -> Float
primitive primRationalToDouble :: Rational -> Double
-----------------------------}

{-----------------------------
-- These functions are used by Hugs - don't change their types.
rationalToFloat  :: Rational -> Float
rationalToDouble :: Rational -> Double
rationalToFloat  = rationalToRealFloat
rationalToDouble = rationalToRealFloat

rationalToRealFloat x = x'
 where x'    = f e
       f e   = if e' == e then y else f e'
               where y      = encodeFloat (round (x * (1%b)^^e)) e
                     (_,e') = decodeFloat y
       (_,e) = decodeFloat (fromInteger (numerator x) `asTypeOf` x'
                             / fromInteger (denominator x))
       b     = floatRadix x'
-----------------------------}

-- primitive Float functions

#if defined( __UHC_TARGET_JAZY__ )
foreign import prim  primSinFloat   :: Float -> Float
foreign import prim  primCosFloat   :: Float -> Float
foreign import prim  primTanFloat   :: Float -> Float
foreign import prim  primAsinFloat  :: Float -> Float
foreign import prim  primAcosFloat  :: Float -> Float
foreign import prim  primAtanFloat  :: Float -> Float
foreign import prim  primExpFloat   :: Float -> Float
foreign import prim  primLogFloat   :: Float -> Float
foreign import prim  primSqrtFloat  :: Float -> Float
foreign import prim  primAtan2Float :: Float -> Float -> Float
#elif defined(__UHC_TARGET_JS__)
foreign import prim "primSinDouble"  primSinFloat   :: Float -> Float
foreign import prim "primCosDouble"  primCosFloat   :: Float -> Float
foreign import prim "primTanDouble"  primTanFloat   :: Float -> Float
foreign import prim "primAsinDouble" primAsinFloat  :: Float -> Float
foreign import prim "primAcosDouble" primAcosFloat  :: Float -> Float
foreign import prim "primAtanDouble" primAtanFloat  :: Float -> Float
foreign import prim "primExpDouble"  primExpFloat   :: Float -> Float
foreign import prim "primLogDouble"  primLogFloat   :: Float -> Float
foreign import prim "primSqrtDouble" primSqrtFloat  :: Float -> Float
foreign import prim "primSinhDouble" primSinhFloat  :: Float -> Float
foreign import prim "primCoshDouble" primCoshFloat  :: Float -> Float
foreign import prim "primTanhDouble" primTanhFloat  :: Float -> Float
foreign import prim "primAtan2Double"  primAtan2Float   :: Float -> Float -> Float
#else 
foreign import ccall "sinf"  primSinFloat   :: Float -> Float
foreign import ccall "cosf"  primCosFloat   :: Float -> Float
foreign import ccall "tanf"  primTanFloat   :: Float -> Float
foreign import ccall "asinf" primAsinFloat  :: Float -> Float
foreign import ccall "acosf" primAcosFloat  :: Float -> Float
foreign import ccall "atanf" primAtanFloat  :: Float -> Float
foreign import ccall "expf"  primExpFloat   :: Float -> Float
foreign import ccall "logf"  primLogFloat   :: Float -> Float
foreign import ccall "sqrtf" primSqrtFloat  :: Float -> Float
-- extra
foreign import ccall "sinhf" primSinhFloat  :: Float -> Float
foreign import ccall "coshf" primCoshFloat  :: Float -> Float
foreign import ccall "tanhf" primTanhFloat  :: Float -> Float
foreign import ccall "atan2f"  primAtan2Float   :: Float -> Float -> Float
#endif

#if defined(__UHC_TARGET_JS__)
foreign import prim "primIsNaNDouble         " primIsNaNFloat              :: Float -> Bool
foreign import prim "primIsNegativeZeroDouble" primIsNegativeZeroFloat     :: Float -> Bool
foreign import prim "primIsDenormalizedDouble" primIsDenormalizedFloat     :: Float -> Bool
foreign import prim "primIsInfiniteDouble    " primIsInfiniteFloat         :: Float -> Bool
foreign import prim "primDigitsDouble        " primDigitsFloat             :: Int
foreign import prim "primMaxExpDouble        " primMaxExpFloat             :: Int
foreign import prim "primMinExpDouble        " primMinExpFloat             :: Int
foreign import prim "primDecodeDouble        " primDecodeFloat             :: Float -> (Integer, Int)
foreign import prim "primEncodeDouble        " primEncodeFloat             :: Integer -> Int -> Float
#else
foreign import prim primIsNaNFloat              :: Float -> Bool
foreign import prim primIsNegativeZeroFloat     :: Float -> Bool
foreign import prim primIsDenormalizedFloat     :: Float -> Bool
foreign import prim primIsInfiniteFloat         :: Float -> Bool
foreign import prim primDigitsFloat             :: Int
foreign import prim primMaxExpFloat             :: Int
foreign import prim primMinExpFloat             :: Int
foreign import prim primDecodeFloat             :: Float -> (Integer, Int)
foreign import prim primEncodeFloat             :: Integer -> Int -> Float
#endif

-- primitive Double functions

#if defined( __UHC_TARGET_JAZY__ ) || defined(__UHC_TARGET_JS__)
foreign import prim  primSinDouble   :: Double -> Double
foreign import prim  primCosDouble   :: Double -> Double
foreign import prim  primTanDouble   :: Double -> Double
foreign import prim  primAsinDouble  :: Double -> Double
foreign import prim  primAcosDouble  :: Double -> Double
foreign import prim  primAtanDouble  :: Double -> Double
foreign import prim  primExpDouble   :: Double -> Double
foreign import prim  primLogDouble   :: Double -> Double
foreign import prim  primSqrtDouble  :: Double -> Double
foreign import prim  primAtan2Double :: Double -> Double -> Double
#if !defined(__UHC_TARGET_JAZY__)
foreign import prim primSinhDouble  :: Double -> Double
foreign import prim primCoshDouble  :: Double -> Double
foreign import prim primTanhDouble  :: Double -> Double
#endif
#else
foreign import ccall "sin"  primSinDouble   :: Double -> Double
foreign import ccall "cos"  primCosDouble   :: Double -> Double
foreign import ccall "tan"  primTanDouble   :: Double -> Double
foreign import ccall "asin" primAsinDouble  :: Double -> Double
foreign import ccall "acos" primAcosDouble  :: Double -> Double
foreign import ccall "atan" primAtanDouble  :: Double -> Double
foreign import ccall "exp"  primExpDouble   :: Double -> Double
foreign import ccall "log"  primLogDouble   :: Double -> Double
foreign import ccall "sqrt" primSqrtDouble  :: Double -> Double
foreign import ccall "sinh" primSinhDouble  :: Double -> Double
foreign import ccall "cosh" primCoshDouble  :: Double -> Double
foreign import ccall "tanh" primTanhDouble  :: Double -> Double
foreign import ccall "atan2"  primAtan2Double   :: Double -> Double -> Double
#endif

foreign import prim primIsNaNDouble             :: Double -> Bool
foreign import prim primIsNegativeZeroDouble    :: Double -> Bool
foreign import prim primIsDenormalizedDouble    :: Double -> Bool
foreign import prim primIsInfiniteDouble        :: Double -> Bool
foreign import prim primDigitsDouble            :: Int
foreign import prim primMaxExpDouble            :: Int
foreign import prim primMinExpDouble            :: Int
foreign import prim primDecodeDouble            :: Double -> (Integer, Int)
foreign import prim primEncodeDouble            :: Integer -> Int -> Double

foreign import prim primIsIEEE  :: Bool
foreign import prim primRadixDoubleFloat  :: Int

#if defined( __UHC_TARGET_JAZY__ ) || defined( __UHC_TARGET_JS__ )
foreign import prim primShowFloatToPackedString :: Float -> PackedString
#else
foreign import prim primShowFloat :: Float -> String
-- TODO: replace this by a function Float -> PackedString
#endif

instance Floating Float where
    exp   = primExpFloat
    log   = primLogFloat
    sqrt  = primSqrtFloat
    sin   = primSinFloat
    cos   = primCosFloat
    tan   = primTanFloat
    asin  = primAsinFloat
    acos  = primAcosFloat
    atan  = primAtanFloat
#ifndef __UHC_TARGET_JAZY__
    sinh  = primSinhFloat
#endif

instance Floating Double where
    exp   = primExpDouble
    log   = primLogDouble
    sqrt  = primSqrtDouble
    sin   = primSinDouble
    cos   = primCosDouble
    tan   = primTanDouble
    asin  = primAsinDouble
    acos  = primAcosDouble
    atan  = primAtanDouble
#if !defined(__UHC_TARGET_JAZY__)
    sinh  = primSinhDouble
    cosh  = primCoshDouble
    tanh  = primTanhDouble
#endif


instance RealFrac Float where
    properFraction = floatProperFraction

instance RealFrac Double where
    properFraction = floatProperFraction

floatProperFraction x
   | n >= 0      = (fromInteger m * fromInteger b ^ n, 0)
   | otherwise   = (fromInteger w, encodeFloat r n)
                   where (m,n) = decodeFloat x
                         b     = floatRadix x
                         (w,r) = quotRem m (b^(-n))

instance RealFloat Float where
    floatRadix  _ = toInteger primRadixDoubleFloat
    floatDigits _ = primDigitsFloat
    floatRange  _ = (primMinExpFloat, primMaxExpFloat)
    encodeFloat   = primEncodeFloat
    decodeFloat   = primDecodeFloat
    isNaN         = primIsNaNFloat
    isInfinite    = primIsInfiniteFloat
    isDenormalized= primIsDenormalizedFloat
    isNegativeZero= primIsNegativeZeroFloat
    isIEEE      _ = primIsIEEE
    atan2         = primAtan2Float

instance RealFloat Double where
    floatRadix  _ = toInteger primRadixDoubleFloat
    floatDigits _ = primDigitsDouble
    floatRange  _ = (primMinExpDouble, primMaxExpDouble)
    encodeFloat   = primEncodeDouble
    decodeFloat   = primDecodeDouble
    isNaN         = primIsNaNDouble
    isInfinite    = primIsInfiniteDouble
    isDenormalized= primIsDenormalizedDouble
    isNegativeZero= primIsNegativeZeroDouble
    isIEEE      _ = primIsIEEE
    atan2         = primAtan2Double

instance Enum Float where
    succ x                = x+1
    pred x                = x-1
    toEnum                = primIntToFloat
    fromEnum              = fromInteger . truncate   -- may overflow
    enumFrom              = numericEnumFrom
    enumFromThen          = numericEnumFromThen
    enumFromTo            = numericEnumFromTo
    enumFromThenTo        = numericEnumFromThenTo

instance Enum Double where
    succ x                = x+1
    pred x                = x-1
    toEnum                = primIntToDouble
    fromEnum              = fromInteger . truncate   -- may overflow
    enumFrom              = numericEnumFrom
    enumFromThen          = numericEnumFromThen
    enumFromTo            = numericEnumFromTo
    enumFromThenTo        = numericEnumFromThenTo

instance Read Float where
    readsPrec p = readSigned readFloat

{-----------------------------
-- Note that showFloat in Numeric isn't used here
instance Show Float where
    showsPrec   = primShowsFloat
-----------------------------}
instance Show Float where
#if defined( __UHC_TARGET_JAZY__ ) || defined( __UHC_TARGET_JS__ )
    show   = packedStringToString . primShowFloatToPackedString
#else
    show   = primShowFloat
#endif

#if defined( __UHC_TARGET_JAZY__ ) || defined( __UHC_TARGET_JS__ )
foreign import prim primShowDoubleToPackedString :: Double -> PackedString
#else
foreign import prim primShowDouble :: Double -> String
#endif

instance Read Double where
    readsPrec p = readSigned readFloat

{-----------------------------
-- Note that showFloat in Numeric isn't used here
instance Show Double where
    showsPrec   = primShowsDouble
-----------------------------}
instance Show Double where
#if defined( __UHC_TARGET_JAZY__ ) || defined( __UHC_TARGET_JS__ )
    show   = packedStringToString . primShowDoubleToPackedString
#else
    show   = primShowDouble
#endif


--------------------------------------------------------------
-- Ratio and Rational type
--------------------------------------------------------------

data Ratio a = !a :% !a deriving (Eq)

type Rational              = Ratio Integer

(%)                       :: Integral a => a -> a -> Ratio a
x % y                      = reduce (x * signum y) (abs y)

reduce                    :: Integral a => a -> a -> Ratio a
reduce x y | y == 0        = error "Ratio.%: zero denominator"
           | otherwise     = (x `quot` d) :% (y `quot` d)
                             where d = gcd x y

numerator, denominator    :: Integral a => Ratio a -> a
numerator (x :% y)         = x
denominator (x :% y)       = y

instance Integral a => Ord (Ratio a) where
    compare (x:%y) (x':%y') = compare (x*y') (x'*y)

instance Integral a => Num (Ratio a) where
    (x:%y) + (x':%y') = reduce (x*y' + x'*y) (y*y')
    (x:%y) * (x':%y') = reduce (x*x') (y*y')
    negate (x :% y)   = negate x :% y
    abs (x :% y)      = abs x :% y
    signum (x :% y)   = signum x :% 1
    fromInteger x     = fromInteger x :% 1
    -- fromInt           = intToRatio
    fromInt x         = fromInt x :% 1

intToRatio :: Integral a => Int -> Ratio a  -- TODO: optimise fromRational (intToRatio x)
intToRatio x = fromInt x :% 1

instance Integral a => Real (Ratio a) where
    toRational (x:%y) = toInteger x :% toInteger y

instance Integral a => Fractional (Ratio a) where
    (x:%y) / (x':%y')   = (x*y') % (y*x')
    recip (x:%y)        = y % x
    fromRational (x:%y) = fromInteger x :% fromInteger y
    fromDouble          = doubleToRatio

doubleToRatio :: Integral a => Double -> Ratio a   -- TODO: optimies fromRational (doubleToRatio x)
doubleToRatio x
            | n>=0      = (round (x / fromInteger pow) * fromInteger pow) % 1
            | otherwise = fromRational (round (x * fromInteger denom) % denom)
                          where (m,n) = decodeFloat x
                                denom, pow, radix :: Integer
                                radix = floatRadix x
                                denom = radix ^ (-n)
                                pow   = radix ^ n

{-
doubleToRatio :: Integral a => Double -> Ratio a   -- TODO: optimies fromRational (doubleToRatio x)
doubleToRatio x
            | n>=0      = (round (x / fromInteger pow) * fromInteger pow) % 1
            | otherwise = fromRational (round (x * fromInteger denom) % denom)
                          where (m,n) = decodeFloat x
                                n_dec, denom, pow :: Integer
                                n_dec = floor (logBase 10 (encodeFloat 1 n :: Double))
                                denom = 10 ^ (-n_dec)
                                pow   = 10 ^ n_dec
-}

instance Integral a => RealFrac (Ratio a) where
    properFraction (x:%y) = (fromIntegral q, r:%y)
                            where (q,r) = quotRem x y

instance Integral a => Enum (Ratio a) where
    succ x         = x+1
    pred x         = x-1
    toEnum         = fromInt
    fromEnum       = fromInteger . truncate   -- may overflow
    enumFrom       = numericEnumFrom
    enumFromTo     = numericEnumFromTo
    enumFromThen   = numericEnumFromThen
    enumFromThenTo = numericEnumFromThenTo

instance (Read a, Integral a) => Read (Ratio a) where
    readsPrec p = readParen (p > 7)
                            (\r -> [(x%y,u) | (x,s)   <- readsPrec 8 r,
                                              ("%",t) <- lex s,
                                              (y,u)   <- readsPrec 8 t ])

instance (Show a,Integral a) => Show (Ratio a) where
    showsPrec p (x:%y) = showParen (p > 7)
                             (showsPrec 8 x . showString " % " . showsPrec 8 y)




--------------------------------------------------------------
-- Some standard functions
--------------------------------------------------------------

--fst            :: (a,b) -> a
fst            :: forall b . (a,b) -> a
fst (x,_)       = x

--snd            :: (a,b) -> b
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


--------------------------------------------------------------
-- Standard list functions
--------------------------------------------------------------


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

length           :: [a] -> Int
length            = foldl' (\n _ -> n + (1::Int)) (0::Int)

(!!)             :: [a] -> Int -> a
xs     !! n | n<0 = error "Prelude.!!: negative index"
[]     !! _       = error "Prelude.!!: index too large"
(x:_)  !! 0       = x
(_:xs) !! n       = xs !! (n-1)

foldl            :: (a -> b -> a) -> a -> [b] -> a
foldl f z []      = z
foldl f z (x:xs)  = foldl f (f z x) xs

foldl'           :: (a -> b -> a) -> a -> [b] -> a
foldl' f a []     = a
#if defined (__UHC_TARGET_C__) || defined (__UHC_TARGET_LLVM__)
foldl' f a (x:xs) = (foldl' f $! f a x) xs
#else
foldl' f a (x:xs) = letstrict fax = f a x in foldl' f fax xs
#endif

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

cycle            :: [a] -> [a]
cycle []          = error "Prelude.cycle: empty list"
cycle xs          = xs' where xs'=xs++xs'

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

sum, product     :: Num a => [a] -> a
sum               = foldl' (+) 0
product           = foldl' (*) 1

maximum, minimum :: Ord a => [a] -> a
maximum           = foldl1 max
minimum           = foldl1 min

concatMap        :: (a -> [b]) -> [a] -> [b]
-- concatMap f       = concat . map f    -- this definition cannot be used, because map is defined as a list comprehension, which is desugared using concatMap
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

--------------------------------------------------------------
-- PreludeText
--------------------------------------------------------------

reads        :: Read a => ReadS a
reads         = readsPrec 0

shows        :: Show a => a -> ShowS
shows         = showsPrec 0

read         :: Read a => String -> a
read s        =  case [x | (x,t) <- reads s, ("","") <- lex t] of
                      [x] -> x
                      []  -> error "Prelude.read: no parse"
                      _   -> error "Prelude.read: ambiguous parse"

showChar     :: Char -> ShowS
showChar      = (:)

showString   :: String -> ShowS
showString    = (++)

showParen    :: Bool -> ShowS -> ShowS
showParen b p = if b then showChar '(' . p . showChar ')' else p

showField    :: Show a => String -> a -> ShowS
showField m@(c:_) v
  | isAlpha c || c == '_' = showString m . showString " = " . shows v
  | otherwise = showChar '(' . showString m . showString ") = " . shows v

readParen    :: Bool -> ReadS a -> ReadS a
readParen b g = if b then mandatory else optional
                where optional r  = g r ++ mandatory r
                      mandatory r = [(x,u) | ("(",s) <- lex r,
                                             (x,t)   <- optional s,
                                             (")",u) <- lex t    ]

readField    :: Read a => String -> ReadS a
readField m s0 = [ r | (t,  s1) <- readFieldName m s0,
                       ("=",s2) <- lex s1,
                       r        <- reads s2 ]

readFieldName :: String -> ReadS String
readFieldName m@(c:_) s0
  | isAlpha c || c == '_' = [ (f,s1) | (f,s1) <- lex s0, f == m ]
  | otherwise = [ (f,s3) | ("(",s1) <- lex s0,
                           (f,s2)   <- lex s1, f == m,
                           (")",s3) <- lex s2 ]

lex                    :: ReadS String
lex ""                  = [("","")]
lex (c:s) | isSpace c   = lex (dropWhile isSpace s)
lex ('\'':s)            = [('\'':ch++"'", t) | (ch,'\'':t)  <- lexLitChar s,
                                               -- head ch /= '\'' || not (null (tail ch))
                                               ch /= "'"                
                          ]
lex ('"':s)             = [('"':str, t)      | (str,t) <- lexString s]
                          where
                          lexString ('"':s) = [("\"",s)]
                          lexString s = [(ch++str, u)
                                                | (ch,t)  <- lexStrItem s,
                                                  (str,u) <- lexString t  ]

                          lexStrItem ('\\':'&':s) = [("\\&",s)]
                          lexStrItem ('\\':c:s) | isSpace c
                              = [("",t) | '\\':t <- [dropWhile isSpace s]]
                          lexStrItem s            = lexLitChar s

lex (c:s) | isSym c     = [(c:sym,t)         | (sym,t) <- [span isSym s]]
          | isAlpha c   = [(c:nam,t)         | (nam,t) <- [span isIdChar s]]
             -- '_' can be the start of a single char or a name/id.
          | c == '_'    = case span isIdChar s of 
                            ([],_) -> [([c],s)]
                            (nm,t) -> [((c:nm),t)]
          | isSingle c  = [([c],s)]
          | isDigit c   = [(c:ds++fe,t)      | (ds,s)  <- [span isDigit s],
                                               (fe,t)  <- lexFracExp s     ]
          | otherwise   = []    -- bad character
                where
                isSingle c  =  c `elem` ",;()[]{}_`"
                isSym c     =  c `elem` "!@#$%&*+./<=>?\\^|:-~"
                isIdChar c  =  isAlphaNum c || c `elem` "_'"

                lexFracExp ('.':c:cs) | isDigit c 
                            = [('.':ds++e,u) | (ds,t) <- lexDigits (c:cs),
                                               (e,u)  <- lexExp t    ]
                lexFracExp s       = lexExp s

                lexExp (e:s) | e `elem` "eE"
                         = [(e:c:ds,u) | (c:t)  <- [s], c `elem` "+-",
                                                   (ds,u) <- lexDigits t] ++
                           [(e:ds,t)   | (ds,t) <- lexDigits s]
                lexExp s = [("",s)]

lexDigits               :: ReadS String
lexDigits               =  nonnull isDigit

nonnull                 :: (Char -> Bool) -> ReadS String
nonnull p s             =  [(cs,t) | (cs@(_:_),t) <- [span p s]]

lexLitChar          :: ReadS String
lexLitChar ""       =  []
lexLitChar (c:s)
 | c /= '\\'        =  [([c],s)]
 | otherwise        =  map (prefix '\\') (lexEsc s)
 where
   lexEsc (c:s)     | c `elem` "abfnrtv\\\"'" = [([c],s)]
   lexEsc ('^':c:s) | c >= '@' && c <= '_'    = [(['^',c],s)]
    -- Numeric escapes
   lexEsc ('o':s)  = [prefix 'o' (span isOctDigit s)]
   lexEsc ('x':s)  = [prefix 'x' (span isHexDigit s)]
   lexEsc s@(c:_) 
     | isDigit c   = [span isDigit s]  
     | isUpper c   = case [(mne,s') | (c, mne) <- table,
                        ([],s') <- [lexmatch mne s]] of
                       (pr:_) -> [pr]
                       []     -> []
   lexEsc _        = []

   table = ('\DEL',"DEL") : asciiTab
   prefix c (t,s) = (c:t, s)

isOctDigit c  =  c >= '0' && c <= '7'
isHexDigit c  =  isDigit c || c >= 'A' && c <= 'F'
                           || c >= 'a' && c <= 'f'

lexmatch                   :: (Eq a) => [a] -> [a] -> ([a],[a])
lexmatch (x:xs) (y:ys) | x == y  =  lexmatch xs ys
lexmatch xs     ys               =  (xs,ys)

asciiTab = zip ['\NUL'..' ']
           (["NUL", "SOH", "STX", "ETX"]++[ "EOT", "ENQ", "ACK", "BEL"]++
           [ "BS",  "HT",  "LF",  "VT" ]++[  "FF",  "CR",  "SO",  "SI"]++
           [ "DLE", "DC1", "DC2", "DC3"]++[ "DC4", "NAK", "SYN", "ETB"]++
           [ "CAN", "EM",  "SUB", "ESC"]++[ "FS",  "GS",  "RS",  "US"]++
           [ "SP"])

readLitChar            :: ReadS Char
readLitChar ('\\':s)    = readEsc s
 where
       readEsc ('a':s)  = [('\a',s)]
       readEsc ('b':s)  = [('\b',s)]
       readEsc ('f':s)  = [('\f',s)]
       readEsc ('n':s)  = [('\n',s)]
       readEsc ('r':s)  = [('\r',s)]
       readEsc ('t':s)  = [('\t',s)]
       readEsc ('v':s)  = [('\v',s)]
       readEsc ('\\':s) = [('\\',s)]
       readEsc ('"':s)  = [('"',s)]
       readEsc ('\'':s) = [('\'',s)]
       readEsc ('^':c:s) | c >= '@' && c <= '_'
                        = [(toEnum (fromEnum c - fromEnum '@'), s)]
       readEsc s@(d:_) | isDigit d
                        = [(toEnum n, t) | (n,t) <- readDec s]
       readEsc ('o':s)  = [(toEnum n, t) | (n,t) <- readOct s]
       readEsc ('x':s)  = [(toEnum n, t) | (n,t) <- readHex s]
       readEsc s@(c:_) | isUpper c
                        = let table = ('\DEL',"DEL") : asciiTab
                          in case [(c,s') | (c, mne) <- table,
                                            ([],s') <- [lexmatch mne s]]
                             of (pr:_) -> [pr]
                                []     -> []
       readEsc _        = []
readLitChar (c:s)       = [(c,s)]

showLitChar               :: Char -> ShowS
showLitChar c | c > '\DEL' = showChar '\\' .
                             protectEsc isDigit (shows (fromEnum c))
showLitChar '\DEL'         = showString "\\DEL"
showLitChar '\\'           = showString "\\\\"
showLitChar c | c >= ' '   = showChar c
showLitChar '\a'           = showString "\\a"
showLitChar '\b'           = showString "\\b"
showLitChar '\f'           = showString "\\f"
showLitChar '\n'           = showString "\\n"
showLitChar '\r'           = showString "\\r"
showLitChar '\t'           = showString "\\t"
showLitChar '\v'           = showString "\\v"
showLitChar '\SO'          = protectEsc ('H'==) (showString "\\SO")
showLitChar c              = showString ('\\' : snd (asciiTab!!fromEnum c))

-- the composition with cont makes CoreToGrin break the GrinModeInvariant so we forget about protecting escapes for the moment
protectEsc p f             = f  -- . cont
 where cont s@(c:_) | p c  = "\\&" ++ s
       cont s              = s

-- Unsigned readers for various bases
readDec, readOct, readHex :: Integral a => ReadS a
readDec = readInt 10 isDigit    (\ d -> fromEnum d - fromEnum_0)
readOct = readInt  8 isOctDigit (\ d -> fromEnum d - fromEnum_0)
readHex = readInt 16 isHexDigit hex
            where hex d = fromEnum d - (if isDigit d then fromEnum_0
                                       else fromEnum (if isUpper d then 'A' else 'a') - 10)

fromEnum_0 :: Int
fromEnum_0 = fromEnum '0'

-- readInt reads a string of digits using an arbitrary base.  
-- Leading minus signs must be handled elsewhere.

readInt :: Integral a => a -> (Char -> Bool) -> (Char -> Int) -> ReadS a
readInt radix isDig digToInt s =
    [(foldl1 (\n d -> n * radix + d) (map (fromIntegral . digToInt) ds), r)
        | (ds,r) <- nonnull isDig s ]

readSigned:: Real a => ReadS a -> ReadS a
readSigned readPos = readParen False read'
                     where read' r  = read'' r ++
                                      [(-x,t) | ("-",s) <- lex r,
                                                (x,t)   <- read'' s]
                           read'' r = [(n,s)  | (str,s) <- lex r,
                                                (n,"")  <- readPos str]

-- This floating point reader uses a less restrictive syntax for floating
-- point than the Haskell lexer.  The `.' is optional.
readFloat     :: RealFrac a => ReadS a
readFloat r    = [(fromRational ((n%1)*10^^(k-d)),t) | (n,d,s) <- readFix r,
                                                       (k,t)   <- readExp s] ++
                 [ (0/0, t) | ("NaN",t)      <- lex r] ++
                 [ (1/0, t) | ("Infinity",t) <- lex r]
                 where readFix r = [(read (ds++ds'), length ds', t)
                                        | (ds, d) <- lexDigits r
                                        , (ds',t) <- lexFrac d   ]

                       lexFrac ('.':s) = lexDigits s
                       lexFrac s       = [("",s)]

                       readExp (e:s) | e `elem` "eE" = readExp' s
                       readExp s                     = [(0::Int,s)]

                       readExp' ('-':s) = [(-k,t) | (k,t) <- readDec s]
                       readExp' ('+':s) = readDec s
                       readExp' s       = readDec s




----------------------------------------------------------------
-- Exception datatype and operations, must match the list in the RTS
----------------------------------------------------------------

data ExitCode = ExitSuccess | ExitFailure Int
                deriving (Eq, Ord, Show)  -- TODO: Read

data SomeException' x                          -- alphabetical order of constructors required, assumed Int encoding in comment
  = ArithException      ArithException      -- 0
  | ArrayException      ArrayException      -- 1
  | AssertionFailed     String              -- 2
  | AsyncException      AsyncException      -- 3
  | BlockedOnDeadMVar                       -- 4
  | Deadlock                                -- 5
  -- | DynException        Dynamic
  | ErrorCall           String              -- 6
  | ExitException       ExitCode            -- 7 
  | IOException         x                   -- 8 -- IO exceptions (from 'ioError')
  | NoMethodError       String              -- 9
  | NonTermination                          -- 10
  | PatternMatchFail    String              -- 11
  | RecConError         String              -- 12
  | RecSelError         String              -- 13
  | RecUpdError         String              -- 14

data ArithException
  = Overflow
  | Underflow
  | LossOfPrecision
  | DivideByZero
  | Denormal
  deriving (Eq, Ord)

data ArrayException
  = IndexOutOfBounds    String
  | UndefinedElement    String
  deriving (Eq, Ord)

data AsyncException
  = HeapOverflow							-- 0
  | StackOverflow		String				-- 1
  | ThreadKilled							-- 2
  deriving (Eq, Ord)


----------------------------------------------------------------
-- Monadic I/O implementation
----------------------------------------------------------------

newtype State s = State s
data RealWorld = RealWorld			-- known to compiler
type IOWorld = State RealWorld

-- newtype IO a = IO (IOWorld -> IOResult a)
newtype IO a = IO {unIO :: (IOWorld -> (IOWorld, a))}
-- newtype IO a = IO (IOWorld -> a)

-- newtype IOResult a = IOResult a

-- Only 1 real world exist, never make a RealWorld elsewhere. Currently it is just there for the type system only.
-- This should be in the RTS... as a primitive?
realWorld :: RealWorld
realWorld = RealWorld
ioWorld :: IOWorld
ioWorld = State realWorld

{-
ioFromPrim :: (IOWorld -> a) -> IO a
ioFromPrim f
  = IO (\w -> letstrict x = f w 
              in (w, x)
       )

primbindIO :: IO a -> (a -> IO b) -> IO b
primbindIO (IO io) f
  = IO (\w -> case io w of
                (w', x) -> letstrict x' = x
                           in case f x' of
                               IO fx -> fx w'
       )
-}

-- This IO variant behaves just like a ST: s -> (s,a), threading a state
ioFromPrim :: (IOWorld -> a) -> IO a
ioFromPrim f
  = IO (\w -> let x = f w
              in  letstrict x' = x   -- as a side effect, this will update x
#ifdef __UHC_TARGET_C__
                  in (w, x)          -- do not use x' here, because the code generated by letstrict violates the Grin-invariant if the result is used directly
#else
                  in (w, x')
#endif
       )

primbindIO :: IO a -> (a -> IO b) -> IO b
primbindIO (IO io) f
  = IO (\w -> case io w of
                (w', x) -> letstrict x' = x    -- as a side effect, this will update x
#ifdef __UHC_TARGET_C__
                           in case f x of      -- do not use x' here, because the code generated by letstrict violates the Grin-invariant if the result is used directly
#else
                           in case f x' of
#endif
                               IO fx -> fx w'
       )

primretIO :: a -> IO a
primretIO x
  = IO (\w -> (w, x))

{-
-- This IO variant only gets an additional state param which triggers the computation, but returns only a result (no tupling with state)
ioFromPrim :: (IOWorld -> a) -> IO a
ioFromPrim f
  = IO (\w -> let x = f w
              in  letstrict x2 = x   -- as a side effect, this will update x
                  in x               -- do not use x2 here, because the code generated by letstrict violates the Grin-invariant if the result is used directly
       )

primbindIO :: IO a -> (a -> IO b) -> IO b
primbindIO (IO io) f
  = IO (\w -> case io w of
                x -> letstrict x' = x    -- as a side effect, this will update x
                     in case f x of      -- do not use x' here, because the code generated by letstrict violates the Grin-invariant if the result is used directly
                         IO fx -> fx w
       )

primretIO :: a -> IO a
primretIO x
  = IO (\w -> x)
-}

instance Functor IO where
    fmap f x = x >>= (return . f)

instance Monad IO where
    (>>=)  = primbindIO
    return = primretIO
    -- fail s = ioError (userError s)




----------------------------------------------------------------
-- exit is also an IO primitive
----------------------------------------------------------------

foreign import prim primExitWith      :: forall a . Int -> a

exitWithIntCode     :: Int -> IO a
exitWithIntCode e   =  ioFromPrim (\_ -> primExitWith e)


%%]

%%[99

----------------------------------------------------------------
-- Defaulting
----------------------------------------------------------------

default Num Integer
default Real Integer
default Enum Integer
default Integral Integer
default Fractional Double
default RealFrac Double
default Floating Double
default RealFloat Double

%%]

%%[99

----------------------------------------------------------------
-- Generics
----------------------------------------------------------------

-- These definitions may only change simultaneously with the compiler.
-- They must be in the first module containing datatypes, because for every datatype these defs are used.
-- However (1): fields may be added with a default, but those will not be known to the compiler.
-- However (2): arguments types of fields are not used, change is therefore also allowed.

--------------------------------------------------------------------------------
-- Representation types
--------------------------------------------------------------------------------

-- | Void: used for datatypes without constructors
#ifdef __UHC__
V1 :: * -> *
#endif
data V1 p

-- | Unit: used for constructors without arguments
#ifdef __UHC__
U1 :: * -> *
#endif
data U1 p = U1

-- | Used for marking occurrences of the parameter
#ifdef __UHC__
Par1 :: * -> *
#endif
newtype Par1 p = Par1 { unPar1 :: p }


-- | Recursive calls of kind * -> *
#ifdef __UHC__
Rec1 :: (* -> *) -> * -> *
#endif
newtype Rec1 f p = Rec1 { unRec1 :: f p }

-- | Constants, additional parameters and recursion of kind *
#ifdef __UHC__
K1 :: * -> * -> * -> *
#endif
newtype K1 i c p = K1 { unK1 :: c }

-- | Meta-information (constructor names, etc.)
#ifdef __UHC__
M1 :: * -> * -> (* -> *) -> * -> *
#endif
newtype M1 i c f p = M1 { unM1 :: f p }

{-
unMeta :: M1 t x f a -> x
unMeta = error "unMeta"
-}

-- | Sums: encode choice between constructors
infixr 5 :+:
#ifdef __UHC__
(:+:) :: (* -> *) -> (* -> *) -> * -> *
#endif
data (:+:) f g p = L1 { unL1 :: f p } | R1 { unR1 :: g p }

-- | Products: encode multiple arguments to constructors
infixr 6 :*:
#ifdef __UHC__
(:*:) :: (* -> *) -> (* -> *) -> * -> *
#endif
data (:*:) f g p = f p :*: g p

-- | Composition of functors
infixr 7 :.:
#ifdef __UHC__
(:.:) :: (* -> *) -> (* -> *) -> * -> *
#endif
newtype (:.:) f g p = Comp1 (f (g p))

-- | Tag for K1: recursion (of kind *)
data R
-- | Tag for K1: parameters (other than the last)
data P

-- | Type synonym for encoding recursion (of kind *)
type Rec0  = K1 R
-- | Type synonym for encoding parameters (other than the last)
type Par0  = K1 P

-- | Tag for M1: datatype
data D
-- | Tag for M1: constructor
data C
-- | Tag for M1: record selector
data S

-- | Type synonym for encoding meta-information for datatypes
type D1 = M1 D

-- | Type synonym for encoding meta-information for constructors
type C1 = M1 C

-- | Type synonym for encoding meta-information for record selectors
type S1 = M1 S

-- | Class for datatypes that represent datatypes
class Datatype d where
  -- | The name of the datatype, fully qualified
#ifdef __UHC__
  datatypeName :: t d f a -> String
  moduleName   :: t d f a -> String
#else
  datatypeName :: t d (f :: * -> *) a -> String
  moduleName   :: t d (f :: * -> *) a -> String
#endif

{-
-- 20100610, AD: This is what the def should be, but run into some trouble wrt polarity, cause yet unknown
class Datatype d where
  -- | The name of the datatype, fully qualified
  datatypeName :: d -> String
  moduleName   :: d -> String

instance Datatype d => Datatype (M1 t d f a) where
  datatypeName = datatypeName . unMeta
  moduleName   = moduleName   . unMeta
-}

-- | Class for datatypes that represent records
class Selector s where
  -- | The name of the selector
  selName :: t s f a -> String

{-
-- 20100610, AD: This is what the def should be, but run into some trouble wrt polarity, cause yet unknown
class Selector s where
  -- | The name of the selector
  selName :: s -> String

instance Selector s => Selector (M1 t s f a) where
  selName = selName . unMeta
-}

-- | Empty selector, for fields without a label

data NoSelector

instance Selector NoSelector where
  selName _ = ""

-- | Class for datatypes that represent data constructors
class Constructor c where
  -- | The name of the constructor
#ifdef __UHC__
  conName :: t c f a -> String
#else
  conName :: t c (f :: * -> *) a -> String
#endif

  -- | The fixity of the constructor
#ifdef __UHC__
  conFixity :: t c f a -> Fixity
#else
  conFixity :: t c (f :: * -> *) a -> Fixity
#endif  
  conFixity = const Prefix

  -- | Marks if this constructor is a record
#ifdef __UHC__
  conIsRecord :: t c f a -> Bool
#else
  conIsRecord :: t c (f :: * -> *) a -> Bool
#endif
  conIsRecord = const False

  -- | Marks if this constructor is a tuple, returning arity >=0 if so, <0 if not
#ifdef __UHC__
  conIsTuple :: t c f a -> Arity
#else
  conIsTuple :: t c (f :: * -> *) a -> Arity
#endif
  conIsTuple = const NoArity
  {-
  -}

{-
-- 20100610, AD: This is what the def should be, but run into some trouble wrt polarity, cause yet unknown
class Constructor c where
  -- | The name of the constructor
  conName :: c -> String

  -- | The fixity of the constructor
  conFixity :: c -> Fixity
  conFixity = const Prefix

  -- | Marks if this constructor is a record
  conIsRecord :: c -> Bool
  conIsRecord = const False

instance Constructor c => Constructor (M1 t c f a) where
  conName     = conName     . unMeta
  conFixity   = conFixity   . unMeta
  conIsRecord = conIsRecord . unMeta
-}

-- | Datatype to represent the arity of a tuple.
data Arity = NoArity | Arity Int
  deriving (Eq, Show, Ord, Read)

-- | Datatype to represent the fixity of a constructor. An infix
-- | declaration directly corresponds to an application of 'Infix'.
data Fixity = Prefix | Infix Associativity Int
  deriving (Eq, Show, Ord, Read)

-- | Datatype to represent the associativy of a constructor
data Associativity =  LeftAssociative 
                   |  RightAssociative
                   |  NotAssociative
  deriving (Eq, Show, Ord, Read)

-- | Representable types of kind *
class Representable0 a rep where
  -- | Convert from the datatype to its representation
  from0  :: a -> rep x
  -- | Convert from the representation to the datatype
  to0    :: rep x -> a

-- | Representable types of kind * -> *
class Representable1 f rep where
  -- | Convert from the datatype to its representation
  from1  :: f a -> rep a
  -- | Convert from the representation to the datatype
  to1    :: rep a -> f a
%%]

%%[99
----------------------------------------------------------------
-- Stacktrace
----------------------------------------------------------------

-- Stacktraces are implicitly referred to from any other module, so must be in the first.

-- For now a stacktrace (i.e. explicit call stack) is simply a list
type ExplicitStackTrace = [String]

-- and the implicit (i.e. runtime stack) stack trace a list of (kind of stack frame, function name)
type ImplicitStackTrace = [(Int,String)]

pushExplicitStackTrace :: String -> ExplicitStackTrace -> ExplicitStackTrace
pushExplicitStackTrace = (:)

%%]

%%[99
--------------------------------------------------------------------------------
-- Generic definition for deriving Bounded
--------------------------------------------------------------------------------

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
--------------------------------------------------------------------------------
-- Generic Eq
--------------------------------------------------------------------------------

class Eq' f where
  geq' :: f a -> f a -> Bool

instance Eq' U1 where
  geq' _ _ = True

instance (Eq c) => Eq' (K1 i c) where
  geq' (K1 a) (K1 b) = a == b

-- No instances for P or Rec because geq is only applicable to types of kind *

instance (Eq' a) => Eq' (M1 i c a) where
  geq' (M1 a) (M1 b) = geq' a b

instance (Eq' a, Eq' b) => Eq' (a :+: b) where
  geq' (L1 a) (L1 b) = geq' a b
  geq' (R1 a) (R1 b) = geq' a b
  geq' _      _      = False

instance (Eq' a, Eq' b) => Eq' (a :*: b) where
  geq' (a1 :*: b1) (a2 :*: b2) = geq' a1 a2 && geq' b1 b2

{-# DERIVABLE Eq (==) geqdefault #-}
-- deriving instance (Eq a) => Eq (Maybe a)
deriving instance (Eq a) => Eq [a]

geqdefault :: (Representable0 a rep0, Eq' rep0) => rep0 x -> a -> a -> Bool
geqdefault (rep :: r) x y = geq' (from0 x :: r) (from0 y :: r)
%%]

%%[99
--------------------------------------------------------------------------------
-- Generic definition for deriving Functor
--------------------------------------------------------------------------------

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

deriving instance Functor Maybe
deriving instance Functor []
%%]

