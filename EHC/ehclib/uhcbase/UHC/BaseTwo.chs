%%[99
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}

module UHC.BaseTwo   -- adapted from the Hugs prelude
(
    Eq         (..),
    Ord        (..),
    Bounded    (..),
    Num        (..),
    Real       (..),
    Integral   (..),
    -- Fractional (..),
    -- Floating   (..),
    -- RealFrac   (..),
    -- RealFloat  (..),
    Enum       (..),
    Functor    (..),
    Monad      (..),
    Show       (..),
    -- Read       (..),

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

    asTypeOf, error, undefined, seq, ($!),
 
 
    -- Bool
    (&&), (||), not, otherwise,
    -- Char
    isSpace, isUpper, isLower, isAlpha, isDigit, 
    -- isOctDigit, isHexDigit, 
    isAlphaNum, showLitChar, 
    -- readLitChar, lexLitChar,
    -- Ratio
    numerator, denominator,
    -- Maybe
    maybe, 
    -- Either
    either,
 
    -- mapM, 
    mapM_, 
    -- sequence, 
    sequence_, (=<<),
    subtract, even, odd, gcd, lcm, (^), 
    -- (^^), 
    absReal, signumReal,
    fromIntegral, 
    -- realToFrac,
    boundedSucc, boundedPred, boundedEnumFrom, boundedEnumFromTo, boundedEnumFromThen, boundedEnumFromThenTo,
    shows, showChar, showString, showParen,
    --reads, read, lex, readParen, readSigned, readInt, readDec, readOct, readHex, readSigned, readFloat, lexDigits, 
    -- reads, read, lex, readParen, readSigned, readInt, readDec, readOct, readHex, readSigned, readFloat, lexDigits, 
    -- fromRat,

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

    ord, chr,

    ioFromPrim,

    unsafeCoerce,

    PackedString,
    packedStringToString, packedStringToInteger, primIntToInteger,
    primGtInt, primEqChar,
    ByteArray,

    -- primEqInt,
    
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

import UHC.BaseOne

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


even, odd        :: (Integral a) => a -> Bool
even n           =  n `rem` 2 == 0
odd              =  not . even

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


type ReadS a = String -> [(a,String)]
type ShowS   = String -> String

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



class Monad m where
    return :: a -> m a
    (>>=)  :: m a -> (a -> m b) -> m b
    (>>)   :: m a -> m b -> m b
    fail   :: String -> m a

    -- Minimal complete definition: (>>=), return
    p >> q  = p >>= \ _ -> q
    fail s  = error s


sequence_        :: Monad m => [m a] -> m ()
sequence_         = foldr (>>) (return ())


mapM_            :: forall a b . Monad m => (a -> m b) -> [a] -> m ()
mapM_ f           = sequence_ . map f

(=<<)            :: Monad m => (a -> m b) -> m a -> m b
f =<< x           = x >>= f

#if defined(__UHC_TARGET_JAZY__) || defined(__UHC_TARGET_CR__)

foreign import prim   primCharToInt   :: Char -> Int
foreign import prim   primIntToChar   :: Int -> Char
#else
foreign import prim "primUnsafeId"  primCharToInt   :: Char -> Int
foreign import prim "primUnsafeId"  primIntToChar   :: Int -> Char
#endif


instance Enum Char where
    toEnum           = primIntToChar
    fromEnum         = primCharToInt


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

ord :: Char -> Int
ord = fromEnum

chr :: Int -> Char
chr = toEnum



instance Monad Maybe where
    Just x  >>= k = k x
    Nothing >>= k = Nothing
    return        = Just
    fail s        = Nothing



instance Monad [ ] where
    (x:xs) >>= f = f x ++ (xs >>= f)
    []     >>= f = []
    return x     = [x]
    fail s       = []


instance Show a => Show [a]  where
    showsPrec p = showList



#if defined (__UHC_TARGET_C__) || defined (__UHC_TARGET_LLVM__)

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

#if defined (__UHC_TARGET_C__) || defined (__UHC_TARGET_LLVM__)


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


data Float     -- opaque datatype of 32bit IEEE floating point numbers
  deriving Generic

data Double    -- opaque datatype of 64bit IEEE floating point numbers
  deriving Generic

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



#if defined( __UHC_TARGET_JAZY__ ) || defined( __UHC_TARGET_JS__ )
foreign import prim primShowFloatToPackedString :: Float -> PackedString
#else
foreign import prim primShowFloat :: Float -> String
#endif

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


instance Show Double where
#if defined( __UHC_TARGET_JAZY__ ) || defined( __UHC_TARGET_JS__ )
    show   = packedStringToString . primShowDoubleToPackedString
#else
    show   = primShowDouble
#endif

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


instance Integral a => Real (Ratio a) where
    toRational (x:%y) = toInteger x :% toInteger y


length           :: [a] -> Int
length            = foldl' (\n _ -> n + (1::Int)) (0::Int)

(!!)             :: [a] -> Int -> a
xs     !! n | n<0 = error "Prelude.!!: negative index"
[]     !! _       = error "Prelude.!!: index too large"
(x:_)  !! 0       = x
(_:xs) !! n       = xs !! (n-1)

foldl'           :: (a -> b -> a) -> a -> [b] -> a
foldl' f a []     = a
#if defined (__UHC_TARGET_C__) || defined (__UHC_TARGET_LLVM__)
foldl' f a (x:xs) = (foldl' f $! f a x) xs
#else
foldl' f a (x:xs) = let !fax = f a x in foldl' f fax xs
#endif

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

sum, product     :: Num a => [a] -> a
sum               = foldl' (+) 0
product           = foldl' (*) 1

shows        :: Show a => a -> ShowS
shows         = showsPrec 0

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


asciiTab = zip ['\NUL'..' ']
           (["NUL", "SOH", "STX", "ETX"]++[ "EOT", "ENQ", "ACK", "BEL"]++
           [ "BS",  "HT",  "LF",  "VT" ]++[  "FF",  "CR",  "SO",  "SI"]++
           [ "DLE", "DC1", "DC2", "DC3"]++[ "DC4", "NAK", "SYN", "ETB"]++
           [ "CAN", "EM",  "SUB", "ESC"]++[ "FS",  "GS",  "RS",  "US"]++
           [ "SP"])

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

protectEsc p f             = f . cont
 where cont s@(c:_) | p c  = "\\&" ++ s
       cont s              = s

fromEnum_0 :: Int
fromEnum_0 = fromEnum '0'

primbindIO :: IO a -> (a -> IO b) -> IO b
primbindIO (IO io) f
  = IO (\w -> case io w of
                (!w', x) -> case f x of
                               IO fx -> fx w'
       )

primretIO :: a -> IO a
primretIO x
  = IO (\w -> (w, x))

instance Functor IO where
    fmap f x = x >>= (return . f)

instance Monad IO where
    (>>=)  = primbindIO
    return = primretIO
    -- fail s = ioError (userError s)

%%]

