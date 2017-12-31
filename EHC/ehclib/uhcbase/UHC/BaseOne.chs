%%[99
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}

module UHC.BaseOne   -- adapted from the Hugs prelude
(
    Eq         (..),
    Ord        (..),
    Bounded    (..),
    Num        (..),
    Real       (..),
    Integral   (..),
    Enum       (..),
    Functor    (..),

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
    IO,
    Rational, 

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
 
    subtract, 
    gcd, 
    absReal, signumReal,
    fromIntegral, 
    boundedSucc, boundedPred, boundedEnumFrom, boundedEnumFromTo, boundedEnumFromThen, boundedEnumFromThenTo,

    fst, snd, curry, uncurry, id, const, (.), flip, ($), until,
    map, (++), concat, filter,
    head, last, tail, init, null, 
    foldl, foldl1, scanl, scanl1, foldr, foldr1, scanr, scanr1,
    iterate, repeat, cycle,
    takeWhile, dropWhile, span, break, lines,
    words, unlines, unwords,
    reverse, and, or,
    any, all, elem, notElem, lookup,
    maximum, minimum, concatMap, 
    zip, zip3, zipWith, zipWith3, unzip, unzip3,


    ioFromPrim,

    unsafeCoerce,

    PackedString,
    packedStringToString, packedStringToInteger, primIntToInteger,
    primGtInt, primEqChar,
    ByteArray,

    
    exitWithIntCode,

    ExplicitStackTrace,
    ImplicitStackTrace,
    pushExplicitStackTrace,

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

  -- * Dummy class
  , Generic

) where

import UHC.BaseZero

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

takeWhile1 :: (a -> Bool) -> [a] -> [a]
takeWhile1 p (x:xs) = x : if p x then takeWhile1 p xs else []

numericEnumFrom        :: Num a => a -> [a]
numericEnumFromThen    :: Num a => a -> a -> [a]
numericEnumFrom n            = iterate' (+1) n
numericEnumFromThen n m      = iterate' (+(m-n)) n

iterate' :: (a -> a) -> a -> [a]        -- strict version of iterate
#if defined (__UHC_TARGET_C__) || defined (__UHC_TARGET_LLVM__)
iterate' f x = x : (iterate' f $! f x)
#else
iterate' f x = x : (let !fx = f x in iterate' f fx)
#endif


class (Eq a) => Num a where
    (+), (-), (*)  :: a -> a -> a
    negate         :: a -> a
    abs, signum    :: a -> a
    fromInteger    :: Integer -> a
    fromInt        :: Int -> a

    x - y           = x + negate y
    negate x        = 0 - x

class (Num a, Ord a) => Real a where
    toRational     :: a -> Rational

class (Real a, Enum a) => Integral a where
    quot, rem, div, mod :: a -> a -> a
    quotRem, divMod     :: a -> a -> (a,a)
    toInteger           :: a -> Integer
    toInt               :: a -> Int

    n `quot` d           = q where (q,r) = quotRem n d
    n `rem` d            = r where (q,r) = quotRem n d
    n `div` d            = q where (q,r) = divMod n d
    n `mod` d            = r where (q,r) = divMod n d
    divMod n d           = if signum r == - signum d then (q-1, r+d) else qr
                           where qr@(q,r) = quotRem n d
    toInt                = toInt . toInteger



subtract       :: Num a => a -> a -> a
subtract        = flip (-)


gcd            :: Integral a => a -> a -> a
gcd 0 0         = error "Prelude.gcd: gcd 0 0 is undefined"
gcd x y         = gcd' (abs x) (abs y)
                  where gcd' x 0 = x
                        gcd' x y = gcd' y (x `rem` y)




fromIntegral   :: (Integral a, Num b) => a -> b
fromIntegral    = fromInteger . toInteger


absReal :: (Ord a,Num a) => a -> a
absReal x    | x >= 0    = x
             | otherwise = -x

signumReal :: (Ord a,Num a) => a -> a
signumReal x | x == 0    =  0
             | x > 0     =  1
             | otherwise = -1




class Enum a where
    succ, pred           :: a -> a
    toEnum               :: Int -> a
    fromEnum             :: a -> Int
    enumFrom             :: a -> [a]              -- [n..]
    enumFromThen         :: a -> a -> [a]         -- [n,m..]
    enumFromTo           :: a -> a -> [a]         -- [n..m]
    enumFromThenTo       :: a -> a -> a -> [a]    -- [n,n'..m]

    succ                  = toEnum . (1+)       . fromEnum
    pred                  = toEnum . subtract 1 . fromEnum
    enumFrom x            = map toEnum [ fromEnum x ..]
    enumFromTo x y        = map toEnum [ fromEnum x .. fromEnum y ]
    enumFromThen x y      = map toEnum [ fromEnum x, fromEnum y ..]
    enumFromThenTo x y z  = map toEnum [ fromEnum x, fromEnum y .. fromEnum z ]


PRIMS_CONVERSION_INTEGER(Int,primIntegerToInt,primIntToInteger)


PRIMS_NUM(Int,primAddInt,primSubInt,primMulInt,primNegInt)


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


foreign import prim primDivInteger  :: Integer -> Integer -> Integer
foreign import prim primModInteger  :: Integer -> Integer -> Integer

#if defined(__UHC_TARGET_JS__)

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

#if defined( __UHC_TARGET_BC__ ) || defined(__UHC_TARGET_JS__) || defined(__UHC_TARGET_CR__)
foreign import prim primQuotRemInteger       :: Integer -> Integer -> (Integer,Integer)
foreign import prim primDivModInteger        :: Integer -> Integer -> (Integer,Integer)
#endif

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



data Ratio a = !a :% !a 
    deriving (Eq)
    -- deriving (Eq, Generic)

type Rational              = Ratio Integer

(%)                       :: Integral a => a -> a -> Ratio a
x % y                      = reduce (x * signum y) (abs y)

reduce                    :: Integral a => a -> a -> Ratio a
reduce x y | y == 0        = error "Ratio.%: zero denominator"
           | otherwise     = (x `quot` d) :% (y `quot` d)
                             where d = gcd x y

%%]

