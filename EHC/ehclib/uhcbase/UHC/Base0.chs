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

module UHC.Base0   -- adapted from the Hugs prelude
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
    -- readFloat, 
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

import UHC.Base5

%%]

