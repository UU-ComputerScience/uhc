%%[99
-- {-# LANGUAGE NoGenericDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}

module UHC.BaseFive   -- adapted from the Hugs prelude
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
    reads, read, lex, readParen, readSigned, readInt, readDec, readOct, readHex, readSigned, 
    readFloat, 
    lexDigits, 
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

  -- * Dummy class
  , Generic
) where

import UHC.BaseFour

#include "IntLikeInstance.h"

-------------------------
-- missing derivings
-------------------------

deriving instance Eq a => Eq (Maybe a)
deriving instance Ord a => Ord (Maybe a)

instance (Eq a, Eq b) => Eq (a,b) where
    (a,b) == (c,d) = a == c && b == d

instance (Ord a, Ord b) => Ord (a,b) where
    (a,b) <= (c,d) = a < c || (a == c && b <= d)

instance Functor Maybe where
    fmap _ Nothing = Nothing
    fmap f (Just a) = Just $ f a

instance Functor [] where
    fmap _ [] = []
    fmap f (x:xs) = f x : fmap f xs

deriving instance Enum Bool
deriving instance Show Bool
deriving instance Read Bool
deriving instance Show a => Show (Maybe a)
deriving instance Read a => Read (Maybe a)
deriving instance (Show a, Show b) => Show (Either a b)
deriving instance Show Ordering
deriving instance Enum Ordering
deriving instance Show ExitCode
deriving instance Show Arity
deriving instance Read Arity
deriving instance Show Fixity
deriving instance Read Fixity
deriving instance Show Associativity
deriving instance Read Associativity

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
iterate' f x = x : (let !fx = f x in iterate' f fx)
#endif

--------------------------------------------------------------
-- Numeric classes: Num, Real, Integral, 
--                  Fractional, Floating, 
--                  RealFrac, RealFloat
--------------------------------------------------------------

--------------------------------------------------------------
-- class Read, Show
--------------------------------------------------------------


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

--------------------------------------------------------------
-- Lists
--------------------------------------------------------------

instance Read a => Read [a]  where
    readsPrec p = readList


--------------------------------------------------------------
-- Int type
--------------------------------------------------------------
-- type Int builtin

instance Read Int where
    readsPrec p = readSigned readDec


instance Read Integer where
    readsPrec p = readSigned readDec


--------------------------------------------------------------
-- Float and Double type
--------------------------------------------------------------

#if defined(__UHC_TARGET_JS__)
foreign import prim "primUnsafeId"       	primIntToFloat          :: Int -> Float
foreign import prim "primUnsafeId"      	primIntToDouble         :: Int -> Double
#else
foreign import prim primIntToFloat          :: Int -> Float
foreign import prim primIntToDouble         :: Int -> Double
#endif


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


instance Read Double where
    readsPrec p = readSigned readFloat

--------------------------------------------------------------
-- Ratio and Rational type
--------------------------------------------------------------

-- intToRatio :: Integral a => Int -> Ratio a  -- TODO: optimise fromRational (intToRatio x)
-- intToRatio x = fromInt x :% 1

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
-- PreludeText
--------------------------------------------------------------

-- readField    :: Read a => String -> ReadS a
-- readField m s0 = [ r | (t,  s1) <- readFieldName m s0,
--                        ("=",s2) <- lex s1,
--                        r        <- reads s2 ]

-- readFieldName :: String -> ReadS String
-- readFieldName m@(c:_) s0
--   | isAlpha c || c == '_' = [ (f,s1) | (f,s1) <- lex s0, f == m ]
--   | otherwise = [ (f,s3) | ("(",s1) <- lex s0,
--                            (f,s2)   <- lex s1, f == m,
--                            (")",s3) <- lex s2 ]

nonnull                 :: (Char -> Bool) -> ReadS String
nonnull p s             =  [(cs,t) | (cs@(_:_),t) <- [span p s]]

asciiTab = zip ['\NUL'..' ']
           (["NUL", "SOH", "STX", "ETX"]++[ "EOT", "ENQ", "ACK", "BEL"]++
           [ "BS",  "HT",  "LF",  "VT" ]++[  "FF",  "CR",  "SO",  "SI"]++
           [ "DLE", "DC1", "DC2", "DC3"]++[ "DC4", "NAK", "SYN", "ETB"]++
           [ "CAN", "EM",  "SUB", "ESC"]++[ "FS",  "GS",  "RS",  "US"]++
           [ "SP"])

lexmatch                   :: (Eq a) => [a] -> [a] -> ([a],[a])
lexmatch (x:xs) (y:ys) | x == y  =  lexmatch xs ys
lexmatch xs     ys               =  (xs,ys)

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

-- -- Unsigned readers for various bases
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

%%]