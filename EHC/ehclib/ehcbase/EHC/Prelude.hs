{----------------------------------------------------------------------------
The EHC prelude is copied/adapted from the Hugs prelude.
----------------------------------------------------------------------------}

module EHC.Prelude (
--  module PreludeList,
{-----------------------------
-----------------------------}
    map, (++), concat, filter,
    head, last, tail, init, null, length, (!!),
    foldl, foldl1, scanl, scanl1, foldr, foldr1, scanr, scanr1,
    iterate, repeat, replicate, cycle,
    take, drop, splitAt, takeWhile, dropWhile, span, break,
    lines, words, unlines, unwords, reverse, and, or,
    any, all, elem, notElem, lookup,
    sum, product, maximum, minimum, concatMap, 
    zip, zip3, zipWith, zipWith3, unzip, unzip3,
--  module PreludeText, 
{-----------------------------
    ReadS, ShowS,
    Read(readsPrec, readList),
    Show(show, showsPrec, showList),
    reads, shows, read, lex,
    showChar, showString, readParen, showParen,
-----------------------------}
    ShowS,
    Show(show, showsPrec, showList),
    shows,
    showChar, showString,
--  module PreludeIO,
{-----------------------------
    FilePath, IOError, ioError, userError, catch,
    putChar, putStr, putStrLn, print,
    getChar, getLine, getContents, interact,
    readFile, writeFile, appendFile, readIO, readLn,
-----------------------------}
    putStr, putStrLn, print,
--  module Ix,
{-----------------------------
    Ix(range, index, unsafeIndex, inRange, rangeSize),
-----------------------------}
--  module Char,
{-----------------------------
-----------------------------}
    isSpace, isUpper, isLower,
    isAlpha, isDigit, isOctDigit, isHexDigit, isAlphaNum,
{-----------------------------
    readLitChar, showLitChar, lexLitChar,
-----------------------------}
    showLitChar,
--  module Numeric
{-----------------------------
    readSigned, readInt,
    readDec, readOct, readHex, readSigned,
    readFloat, lexDigits, 
-----------------------------}
--  module Ratio,
{-----------------------------
    Ratio((:%)), (%), numerator, denominator,
-----------------------------}
--  Non-standard exports
    IO(..), IOResult(..),
    IOException(..), IOErrorType(..),
    Exception(..),
    ArithException(..), ArrayException(..), AsyncException(..),
    ExitCode(..),
    FunPtr, Ptr, Addr,
    Word, StablePtr, ForeignObj, ForeignPtr,
    Int8, Int16, Int32, Int64,
    Word8, Word16, Word32, Word64,
    Handle, Object,
{-----------------------------
    basicIORun, blockIO, IOFinished(..),
    threadToIOResult,
    catchException, throw,
    Dynamic(..), TypeRep(..), Key(..), TyCon(..), Obj,

    IOMode(..),
-----------------------------}
    stdin, stdout, stderr,
{-----------------------------
    openFile,
    hClose,
    hGetContents, hGetChar, hGetLine,
    hPutChar,
-----------------------------}
    hPutStr, hPutStrLn,
    hFlush,

{-----------------------------
-----------------------------}
    Bool(False, True),
    Maybe(Nothing, Just),
    Either(Left, Right),
    Ordering(LT, EQ, GT),
    Char, String, Int, Integer, Float, Double, Rational, IO,

    packedStringToString,
    packedStringToInteger,
    
    primAddInt,
    
--  List type: []((:), [])
{-----------------------------
-----------------------------}
    ''[]''(..),
--  Tuple types: (,), (,,), etc.
{-----------------------------
-----------------------------}
--  Trivial type: ()
{-----------------------------
-----------------------------}
--  Functions: (->)
{-----------------------------
    Rec, emptyRec, EmptyRow, -- non-standard, should only be exported if TREX
-----------------------------}
    Eq((==), (/=)),
    Ord(compare, (<), (<=), (>=), (>), max, min),
    Enum(succ, pred, toEnum, fromEnum, enumFrom, enumFromThen,
         enumFromTo, enumFromThenTo),
    Bounded(minBound, maxBound),
--  Num((+), (-), (*), negate, abs, signum, fromInteger),
{-----------------------------
-----------------------------}
    Num((+), (-), (*), negate, abs, signum, fromInteger, fromInt),
    Real(toRational),
--  Integral(quot, rem, div, mod, quotRem, divMod, toInteger),
{-----------------------------
-----------------------------}
    Integral(quot, rem, div, mod, quotRem, divMod, toInteger, toInt),
--  Fractional((/), recip, fromRational),
{-----------------------------
    Fractional((/), recip, fromRational, fromDouble),
    Floating(pi, exp, log, sqrt, (**), logBase, sin, cos, tan,
             asin, acos, atan, sinh, cosh, tanh, asinh, acosh, atanh),
    RealFrac(properFraction, truncate, round, ceiling, floor),
    RealFloat(floatRadix, floatDigits, floatRange, decodeFloat,
              encodeFloat, exponent, significand, scaleFloat, isNaN,
              isInfinite, isDenormalized, isIEEE, isNegativeZero, atan2),
-----------------------------}
    Monad((>>=), (>>), return, fail),
{-----------------------------
-----------------------------}
    Functor(fmap),
    mapM, mapM_, sequence, sequence_, (=<<),
    maybe, either,
    (&&), (||), not, otherwise,
    subtract, even, odd, gcd, lcm, (^), (^^),
{-----------------------------
    fromIntegral, realToFrac,
-----------------------------}
    fst, snd, curry, uncurry, id, const, (.), flip, ($), until,
    asTypeOf, error, undefined,
    seq, ($!),

{-----------------------------
-----------------------------}
    boundedSucc,
    boundedPred,
    boundedEnumFrom,
    boundedEnumFromTo,
    boundedEnumFromThen,
    boundedEnumFromThenTo
  ) where

-- Standard value bindings {Prelude} ----------------------------------------

infixr 9  .
infixl 9  !!
infixr 8  ^, ^^, **
infixl 7  *, /, `quot`, `rem`, `div`, `mod`, :%, %
infixl 6  +, -
--infixr 5  :    -- this fixity declaration is hard-wired into Hugs
infixr 5  :    -- this fixity declaration is not hard-wired into EHC
infixr 5  ++
infix  4  ==, /=, <, <=, >=, >, `elem`, `notElem`
infixr 3  &&
infixr 2  ||
infixl 1  >>, >>=
infixr 1  =<<
infixr 0  $, $!, `seq`

-- Equality and Ordered classes ---------------------------------------------

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

-- Numeric classes ----------------------------------------------------------

{-----------------------------
class (Eq a, Show a) => Num a where
-----------------------------}
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
    fromDouble    = fromRational . fromDouble
    x / y         = x * recip y


{-----------------------------
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
-----------------------------}

-- Numeric functions --------------------------------------------------------

subtract       :: Num a => a -> a -> a
subtract        = flip (-)

{-----------------------------
-----------------------------}
even, odd        :: (Integral a) => a -> Bool
even n           =  n `rem` 2 == 0
odd              =  not . even

gcd            :: Integral a => a -> a -> a
gcd 0 0         = error "Prelude.gcd: gcd 0 0 is undefined"
gcd x y         = gcd' (abs x) (abs y)
                  where gcd' x 0 = x
                        gcd' x y = gcd' y (x `rem` y)
{-----------------------------
gcd            :: Integral a => a -> a -> a
gcd x y | x == y && x == 0
                = error "Prelude.gcd: gcd 0 0 is undefined"
        | True  = gcd' (abs x) (abs y)
                  where gcd' x y | y == 0 = x
                                 | True   = gcd' y (x `rem` y)

-----------------------------}
lcm            :: (Integral a) => a -> a -> a
lcm _ 0         = 0
lcm 0 _         = 0
lcm x y         = abs ((x `quot` gcd x y) * y)

{-----------------------------
-----------------------------}
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

{-----------------------------
realToFrac     :: (Real a, Fractional b) => a -> b
realToFrac      = fromRational . toRational
-----------------------------}

-- Index and Enumeration classes --------------------------------------------

{-----------------------------
class (Ord a) => Ix a where
    range                :: (a,a) -> [a]
        -- The unchecked variant unsafeIndex is non-standard, but useful
    index, unsafeIndex   :: (a,a) -> a -> Int
    inRange              :: (a,a) -> a -> Bool
    rangeSize            :: (a,a) -> Int

        -- Must specify one of index, unsafeIndex
    index b i | inRange b i = unsafeIndex b i
              | otherwise   = error "Ix.index: index out of range"
    unsafeIndex b i = index b i

    rangeSize b@(_l,h) | inRange b h = unsafeIndex b h + 1
                       | otherwise   = 0
        -- NB: replacing "inRange b h" by  "l <= u"
        -- fails if the bounds are tuples.  For example,
        --      (1,2) <= (2,1)
        -- but the range is nevertheless empty
        --      range ((1,2),(2,1)) = []
-----------------------------}

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

-- Read and Show classes ------------------------------------------------------

{-----------------------------
type ReadS a = String -> [(a,String)]
-----------------------------}
type ShowS   = String -> String

{-----------------------------
class Read a where
    readsPrec :: Int -> ReadS a
    readList  :: ReadS [a]

    -- Minimal complete definition: readsPrec
    readList   = readParen False (\r -> [pr | ("[",s) <- lex r,
                                              pr      <- readl s ])
                 where readl  s = [([],t)   | ("]",t) <- lex s] ++
                                  [(x:xs,u) | (x,t)   <- reads s,
                                              (xs,u)  <- readl' t]
                       readl' s = [([],t)   | ("]",t) <- lex s] ++
                                  [(x:xs,v) | (",",t) <- lex s,
                                              (x,u)   <- reads t,
                                              (xs,v)  <- readl' u]
-----------------------------}

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

-- Monad classes ------------------------------------------------------------

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

sequence_        :: Monad m => [m a] -> m ()
sequence_         = foldr (>>) (return ())

mapM             :: Monad m => (a -> m b) -> [a] -> m [b]
mapM f            = sequence . map f

mapM_            :: Monad m => (a -> m b) -> [a] -> m ()
mapM_ f           = sequence_ . map f

(=<<)            :: Monad m => (a -> m b) -> m a -> m b
f =<< x           = x >>= f

-- Evaluation and strictness ------------------------------------------------

{-----------------------------
primitive seq           :: a -> b -> b

primitive ($!) "strict" :: (a -> b) -> a -> b
-- f $! x                = x `seq` f x
-----------------------------}

seq :: a -> b -> b -- forall a . a -> forall b . b -> b
x `seq` y = letstrict x' = x in y

f $! x                = x `seq` f x

-- Trivial type -------------------------------------------------------------

-- data () = () deriving (Eq, Ord, Ix, Enum, Read, Show, Bounded)

{-----------------------------
instance Eq () where
    () == ()  =  True

instance Ord () where
    compare () () = EQ

instance Ix () where
    range ((),())      = [()]
    index ((),()) ()   = 0
    inRange ((),()) () = True

instance Enum () where
    toEnum 0           = ()
    fromEnum ()        = 0
    enumFrom ()        = [()]

instance Read () where
    readsPrec p = readParen False (\r -> [((),t) | ("(",s) <- lex r,
                                                   (")",t) <- lex s ])

instance Show () where
    showsPrec p () = showString "()"

instance Bounded () where
    minBound = ()
    maxBound = ()
-----------------------------}

-- Boolean type -------------------------------------------------------------

data Bool    = False | True
{-----------------------------
               deriving (Eq, Ord, Ix, Enum, Read, Show, Bounded)
-----------------------------}
               deriving (Eq, Ord, Enum, Show)

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

-- Character type -----------------------------------------------------------

{-----------------------------
data Char               -- builtin datatype of ISO Latin characters
-----------------------------}
type String = [Char]    -- strings are lists of characters

{-----------------------------
primitive primEqChar    :: Char -> Char -> Bool
primitive primCmpChar   :: Char -> Char -> Ordering
-----------------------------}
foreign import ccall "primEqInt"   primEqChar    :: Char -> Char -> Bool
foreign import ccall "primCmpInt"  primCmpChar   :: Char -> Char -> Ordering

{-----------------------------
-----------------------------}
instance Eq Char  where (==)    = primEqChar
instance Ord Char where compare = primCmpChar

{-----------------------------
primitive primCharToInt :: Char -> Int
primitive primIntToChar :: Int -> Char
-----------------------------}
foreign import ccall "primUnsafeId"  primCharToInt   :: Char -> Int
foreign import ccall "primUnsafeId"  primIntToChar   :: Int -> Char

instance Enum Char where
    toEnum           = primIntToChar
    fromEnum         = primCharToInt
{-----------------------------
    enumFrom c       = map toEnum [fromEnum c .. fromEnum (maxBound::Char)]
    enumFromThen     = boundedEnumFromThen
-----------------------------}

{-----------------------------
instance Ix Char where
    range (c,c')      = [c..c']
    unsafeIndex (c,_) i = fromEnum i - fromEnum c
    inRange (c,c') i  = c <= i && i <= c'

instance Read Char where
    readsPrec p      = readParen False
                            (\r -> [(c,t) | ('\'':s,t) <- lex r,
                                            (c,"\'")   <- readLitChar s ])
    readList = readParen False (\r -> [(l,t) | ('"':s, t) <- lex r,
                                               (l,_)      <- readl s ])
               where readl ('"':s)      = [("",s)]
                     readl ('\\':'&':s) = readl s
                     readl s            = [(c:cs,u) | (c ,t) <- readLitChar s,
-----------------------------}

{-----------------------------
-----------------------------}
instance Show Char where
    showsPrec p '\'' = showString "'\\''"
    showsPrec p c    = showChar '\'' . showLitChar c . showChar '\''

    showList cs   = showChar '"' . showl cs
                    where showl ""       = showChar '"'
                          showl ('"':cs) = showString "\\\"" . showl cs
                          showl (c:cs)   = showLitChar c . showl cs

{-----------------------------
-----------------------------}
instance Bounded Char where
    minBound = '\0'
    maxBound = '\xff' -- primMaxChar

{-----------------------------
primitive primMaxChar :: Char
-----------------------------}

isSpace, isDigit :: Char -> Bool

isSpace c              =  c == ' '  ||
                          c == '\t' ||
                          c == '\n' ||
                          c == '\r' ||
                          c == '\f' ||
                          c == '\v' ||
                          c == '\xa0'

isDigit c              =  c >= '0'   &&  c <= '9'

{-----------------------------
primitive isUpper      :: Char -> Bool
primitive isLower      :: Char -> Bool
primitive isAlpha      :: Char -> Bool
primitive isAlphaNum   :: Char -> Bool
-----------------------------}
foreign import ccall "primCharIsUpper"   isUpper    :: Char -> Bool
foreign import ccall "primCharIsLower"   isLower    :: Char -> Bool

isAlpha c = isUpper c || isLower c
isAlphaNum c = isAlpha c || isDigit c

-- Maybe type ---------------------------------------------------------------

data Maybe a = Nothing | Just a
{-----------------------------
               deriving (Eq, Ord, Read, Show)
-----------------------------}
               deriving (Eq, Ord, Show)

maybe             :: b -> (a -> b) -> Maybe a -> b
maybe n f Nothing  = n
maybe n f (Just x) = f x

instance Functor Maybe where
    fmap f Nothing  = Nothing
    fmap f (Just x) = Just (f x)

instance Monad Maybe where
    Just x  >>= k = k x
    Nothing >>= k = Nothing
    return        = Just
    fail s        = Nothing

-- Either type --------------------------------------------------------------

data Either a b = Left a | Right b
{-----------------------------
                  deriving (Eq, Ord, Read, Show)
-----------------------------}
                  deriving (Eq, Ord, Show)

either              :: (a -> c) -> (b -> c) -> Either a b -> c
either l r (Left x)  = l x
either l r (Right y) = r y

-- Ordering type ------------------------------------------------------------

data Ordering = LT | EQ | GT
{-----------------------------
                deriving (Eq, Ord, Ix, Enum, Read, Show, Bounded)
-----------------------------}
                deriving (Eq, Ord, Enum, Show)

-- Lists --------------------------------------------------------------------

{-----------------------------
-- data [a] = [] | a : [a] deriving (Eq, Ord)
-----------------------------}
data [] a = ''[]'' | a : [a]

instance Eq a => Eq [a] where
    []     == []     =  True
    (x:xs) == (y:ys) =  x==y && xs==ys
    _      == _      =  False

instance Ord a => Ord [a] where
    compare []     (_:_)  = LT
    compare []     []     = EQ
    compare (_:_)  []     = GT
    compare (x:xs) (y:ys) = primCompAux x y (compare xs ys)

instance Functor [] where
    fmap = map

instance Monad [ ] where
    (x:xs) >>= f = f x ++ (xs >>= f)
    []     >>= f = []
    return x     = [x]
    fail s       = []

{-----------------------------
instance Read a => Read [a]  where
    readsPrec p = readList
-----------------------------}

instance Show a => Show [a]  where
    showsPrec p = showList

-- Tuples -------------------------------------------------------------------

-- data (a,b) = (a,b) deriving (Eq, Ord, Ix, Read, Show)
-- etc..

-- Standard Integral types --------------------------------------------------

{-----------------------------
data Int      -- builtin datatype of fixed size integers
data Integer  -- builtin datatype of arbitrary size integers
-----------------------------}

{-----------------------------
primitive primEqInt      :: Int -> Int -> Bool
primitive primCmpInt     :: Int -> Int -> Ordering
primitive primEqInteger  :: Integer -> Integer -> Bool
primitive primCmpInteger :: Integer -> Integer -> Ordering
-----------------------------}

foreign import ccall primGtInt      :: Int -> Int -> Bool
foreign import ccall primLtInt      :: Int -> Int -> Bool
foreign import ccall primEqInt      :: Int -> Int -> Bool
foreign import ccall primEqInteger  :: Integer -> Integer -> Bool
foreign import ccall primCmpInt     :: Int -> Int -> Ordering
foreign import ccall primCmpInteger :: Integer -> Integer -> Ordering

instance Eq  Int     where (==)    = primEqInt
instance Eq  Integer where (==)    = primEqInteger
instance Ord Int     where
  compare = primCmpInt
  (<)     = primLtInt
  (>)     = primGtInt
instance Ord Integer where compare = primCmpInteger
{-----------------------------
-----------------------------}

{-----------------------------
primitive primPlusInt,
          primMinusInt,
          primMulInt       :: Int -> Int -> Int
primitive primNegInt       :: Int -> Int
primitive primIntegerToInt :: Integer -> Int
-----------------------------}
foreign import ccall primAddInt       :: Int -> Int -> Int
foreign import ccall primSubInt       :: Int -> Int -> Int
foreign import ccall primMulInt       :: Int -> Int -> Int
foreign import ccall primNegInt       :: Int -> Int
foreign import ccall primIntegerToInt :: Integer -> Int

{-----------------------------
instance Num Int where
    (+)           = primPlusInt
    (-)           = primMinusInt
    negate        = primNegInt
    (*)           = primMulInt
    abs           = absReal
    signum        = signumReal
    fromInteger   = primIntegerToInt
    fromInt x     = x
-----------------------------}
instance Num Int where
    (+)           = primAddInt
    (-)           = primSubInt
    negate        = primNegInt
    (*)           = primMulInt
    abs           = absReal
    signum        = signumReal
    fromInteger   = primIntegerToInt
    fromInt x     = x

{-----------------------------
primitive primMinInt, primMaxInt :: Int
-----------------------------}
foreign import ccall primMaxInt :: Int
foreign import ccall primMinInt :: Int

{-----------------------------
-----------------------------}
instance Bounded Int where
    minBound = primMinInt
    maxBound = primMaxInt

{-----------------------------
primitive primPlusInteger,
          primMinusInteger,
          primMulInteger   :: Integer -> Integer -> Integer
primitive primNegInteger   :: Integer -> Integer
primitive primIntToInteger :: Int -> Integer
-----------------------------}
foreign import ccall primAddInteger       :: Integer -> Integer -> Integer
foreign import ccall primSubInteger       :: Integer -> Integer -> Integer
foreign import ccall primMulInteger       :: Integer -> Integer -> Integer
foreign import ccall primNegInteger       :: Integer -> Integer
foreign import ccall primIntToInteger     :: Int -> Integer

{-----------------------------
instance Num Integer where
    (+)           = primPlusInteger
    (-)           = primMinusInteger
    negate        = primNegInteger
    (*)           = primMulInteger
    abs           = absReal
    signum        = signumReal
    fromInteger x = x
    fromInt       = primIntToInteger
-----------------------------}
instance Num Integer where
    (+)           = primAddInteger
    (-)           = primSubInteger
    negate        = primNegInteger
    (*)           = primMulInteger
    abs           = absReal
    signum        = signumReal
    fromInteger x = x
    fromInt       = primIntToInteger

{-----------------------------
absReal x    | x >= 0    = x
             | otherwise = -x

signumReal x | x == 0    =  0
             | x > 0     =  1
             | otherwise = -1
-----------------------------}
absReal :: (Ord a,Num a) => a -> a
absReal x    | x >= 0    = x
             | otherwise = -x

signumReal :: (Ord a,Num a) => a -> a
signumReal x | x == 0    =  0
             | x > 0     =  1
             | otherwise = -1

{-----------------------------
-----------------------------}
instance Real Int where
    toRational x = toInteger x % 1

instance Real Integer where
    toRational x = x % 1

{-----------------------------
primitive primDivInt,
          primQuotInt,
          primRemInt,
          primModInt  :: Int -> Int -> Int
primitive primQrmInt  :: Int -> Int -> (Int,Int)
-----------------------------}
foreign import ccall primDivInt       :: Int -> Int -> Int
foreign import ccall primModInt       :: Int -> Int -> Int
foreign import ccall primQuotInt      :: Int -> Int -> Int
foreign import ccall primRemInt       :: Int -> Int -> Int

instance Integral Int where
    div       = primDivInt
    quot      = primQuotInt
    rem       = primRemInt
    mod       = primModInt
{-----------------------------
    quotRem   = primQrmInt
-----------------------------}
    toInteger = primIntToInteger
    toInt x   = x

{-----------------------------
primitive primQrmInteger  :: Integer -> Integer -> (Integer,Integer)
-----------------------------}
foreign import ccall primQuotInteger          :: Integer -> Integer -> Integer
foreign import ccall primRemInteger           :: Integer -> Integer -> Integer
foreign import ccall primQuotRemInteger       :: Integer -> Integer -> (Integer,Integer)
foreign import ccall primDivInteger           :: Integer -> Integer -> Integer
foreign import ccall primModInteger           :: Integer -> Integer -> Integer
foreign import ccall primDivModInteger        :: Integer -> Integer -> (Integer,Integer)

{-----------------------------
instance Integral Integer where
    quotRem     = primQrmInteger
    toInteger x = x
    toInt       = primIntegerToInt
-----------------------------}
instance Integral Integer where
    divMod      = primDivModInteger
    quotRem     = primQuotRemInteger
    div         = primDivInteger
    mod         = primModInteger
    quot        = primQuotInteger
    rem         = primRemInteger
    toInteger x = x
    toInt       = primIntegerToInt

{-----------------------------
instance Ix Int where
    range (m,n)          = [m..n]
    unsafeIndex (m,_) i  = i - m
    inRange (m,n) i      = m <= i && i <= n

instance Ix Integer where
    range (m,n)          = [m..n]
    unsafeIndex (m,_) i  = toInt (i - m)
    inRange (m,n) i      = m <= i && i <= n
-----------------------------}

{-----------------------------
-----------------------------}
instance Enum Int where
    succ           = boundedSucc
    pred           = boundedPred
    toEnum         = id
    fromEnum       = id
    enumFrom       = boundedEnumFrom
    enumFromTo     = boundedEnumFromTo
    enumFromThen   = boundedEnumFromThen
    enumFromThenTo = boundedEnumFromThenTo

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

{-----------------------------
-----------------------------}
-- takeWhile and one more
takeWhile1 :: (a -> Bool) -> [a] -> [a]
takeWhile1 p (x:xs) = x : if p x then takeWhile1 p xs else []

{-----------------------------
-----------------------------}
instance Enum Integer where
    succ x         = x + 1
    pred x         = x - 1

    toEnum         = primIntToInteger
    fromEnum       = primIntegerToInt
    enumFrom       = numericEnumFrom
    enumFromThen   = numericEnumFromThen
    enumFromTo n m = takeWhile (<= m) (numericEnumFrom n)
    enumFromThenTo n n' m = takeWhile p (numericEnumFromThen n n')
                                where p | n' >= n   = (<= m)
                                        | otherwise = (>= m)

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
iterate' f x = x : (iterate' f $! f x)

{-----------------------------
primitive primShowsInt :: Int -> Int -> ShowS
-----------------------------}
foreign import ccall primShowInt :: Int -> String

{-----------------------------
instance Read Int where
    readsPrec p = readSigned readDec
-----------------------------}

{-----------------------------
instance Show Int where
    showsPrec   = primShowsInt
-----------------------------}
instance Show Int where
    show   = primShowInt

{-----------------------------
primitive primShowsInteger :: Int -> Integer -> ShowS
-----------------------------}

{-----------------------------
instance Read Integer where
    readsPrec p = readSigned readDec

instance Show Integer where
    showsPrec   = primShowsInteger
-----------------------------}

-- Standard Floating types --------------------------------------------------

{-----------------------------
-----------------------------}
data Float     -- builtin datatype of single precision floating point numbers
data Double    -- builtin datatype of double precision floating point numbers

{-----------------------------
primitive primEqFloat   :: Float -> Float -> Bool
primitive primCmpFloat  :: Float -> Float -> Ordering
primitive primEqDouble  :: Double -> Double -> Bool
primitive primCmpDouble :: Double -> Double -> Ordering
-----------------------------}
foreign import ccall primEqFloat   :: Float -> Float -> Bool
foreign import ccall primCmpFloat  :: Float -> Float -> Ordering
foreign import ccall primEqDouble  :: Double -> Double -> Bool
foreign import ccall primCmpDouble :: Double -> Double -> Ordering

{-----------------------------
-----------------------------}
instance Eq  Float  where (==) = primEqFloat
instance Eq  Double where (==) = primEqDouble

instance Ord Float  where compare = primCmpFloat
instance Ord Double where compare = primCmpDouble

{-----------------------------
primitive primPlusFloat,
          primMinusFloat,
          primMulFloat       :: Float -> Float -> Float
primitive primNegFloat       :: Float -> Float
primitive primIntToFloat     :: Int -> Float
primitive primIntegerToFloat :: Integer -> Float
-----------------------------}
foreign import ccall primAddFloat       :: Float -> Float -> Float
foreign import ccall primSubFloat       :: Float -> Float -> Float
foreign import ccall primMulFloat       :: Float -> Float -> Float
foreign import ccall primNegFloat       :: Float -> Float
foreign import ccall primIntToFloat     :: Int -> Float
foreign import ccall primIntegerToFloat :: Integer -> Float

{-----------------------------
instance Num Float where
    (+)           = primPlusFloat
    (-)           = primMinusFloat
    negate        = primNegFloat
    (*)           = primMulFloat
    abs           = absReal
    signum        = signumReal
    fromInteger   = primIntegerToFloat
    fromInt       = primIntToFloat
-----------------------------}
instance Num Float where
    (+)           = primAddFloat
    (-)           = primSubFloat
    negate        = primNegFloat
    (*)           = primMulFloat
    abs           = absReal
    signum        = signumReal
    fromInteger   = primIntegerToFloat
    fromInt       = primIntToFloat

{-----------------------------
primitive primPlusDouble,
          primMinusDouble,
          primMulDouble       :: Double -> Double -> Double
primitive primNegDouble       :: Double -> Double
primitive primIntToDouble     :: Int -> Double
primitive primIntegerToDouble :: Integer -> Double
-----------------------------}
foreign import ccall primAddDouble       :: Double -> Double -> Double
foreign import ccall primSubDouble       :: Double -> Double -> Double
foreign import ccall primMulDouble       :: Double -> Double -> Double
foreign import ccall primNegDouble       :: Double -> Double
foreign import ccall primIntToDouble     :: Int -> Double
foreign import ccall primIntegerToDouble :: Integer -> Double

{-----------------------------
instance Num Double where
    (+)         = primPlusDouble
    (-)         = primMinusDouble
    negate      = primNegDouble
    (*)         = primMulDouble
    abs         = absReal
    signum      = signumReal
    fromInteger = primIntegerToDouble
    fromInt     = primIntToDouble
-----------------------------}
instance Num Double where
    (+)         = primAddDouble
    (-)         = primSubDouble
    negate      = primNegDouble
    (*)         = primMulDouble
    abs         = absReal
    signum      = signumReal
    fromInteger = primIntegerToDouble
    fromInt     = primIntToDouble

{-----------------------------
instance Real Float where
    toRational = floatToRational

instance Real Double where
    toRational = doubleToRational
-----------------------------}

{-----------------------------
-- Calls to these functions are optimised when passed as arguments to
-- fromRational.
floatToRational  :: Float  -> Rational
doubleToRational :: Double -> Rational
floatToRational  x = realFloatToRational x 
doubleToRational x = realFloatToRational x

realFloatToRational x = (m%1)*(b%1)^^n
                        where (m,n) = decodeFloat x
                              b     = floatRadix x
-----------------------------}

{-----------------------------
primitive primDivFloat      :: Float -> Float -> Float
primitive primDoubleToFloat :: Double -> Float
primitive primFloatToDouble :: Float -> Double  -- used by runtime optimizer
-----------------------------}
foreign import ccall primDivFloat      :: Float -> Float -> Float
foreign import ccall primDoubleToFloat :: Double -> Float
-- foreign import ccall primFloatToDouble :: Float -> Double  -- used by runtime optimizer

{-----------------------------
-----------------------------}
instance Fractional Float where
    (/)          = primDivFloat
    fromRational = primRationalToFloat
    fromDouble   = primDoubleToFloat

{-----------------------------
primitive primDivDouble :: Double -> Double -> Double
-----------------------------}
foreign import ccall primDivDouble :: Double -> Double -> Double

{-----------------------------
-----------------------------}
instance Fractional Double where
    (/)          = primDivDouble
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
foreign import ccall primRationalToFloat  :: Rational -> Float
foreign import ccall primRationalToDouble :: Rational -> Double

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

{-----------------------------
primitive primSinFloat,  primAsinFloat, primCosFloat,
          primAcosFloat, primTanFloat,  primAtanFloat,
          primLogFloat,  primExpFloat,  primSqrtFloat :: Float -> Float
-----------------------------}

{-----------------------------
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
-----------------------------}

{-----------------------------
primitive primSinDouble,  primAsinDouble, primCosDouble,
          primAcosDouble, primTanDouble,  primAtanDouble,
          primLogDouble,  primExpDouble,  primSqrtDouble :: Double -> Double
-----------------------------}

{-----------------------------
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

instance RealFrac Float where
    properFraction = floatProperFraction

instance RealFrac Double where
    properFraction = floatProperFraction
-----------------------------}

{-----------------------------
floatProperFraction x
   | n >= 0      = (fromInteger m * fromInteger b ^ n, 0)
   | otherwise   = (fromInteger w, encodeFloat r n)
                   where (m,n) = decodeFloat x
                         b     = floatRadix x
                         (w,r) = quotRem m (b^(-n))
-----------------------------}

{-----------------------------
primitive primFloatRadix  :: Integer
primitive primFloatDigits :: Int
primitive primFloatMinExp,
          primFloatMaxExp :: Int
primitive primFloatEncode :: Integer -> Int -> Float
primitive primFloatDecode :: Float -> (Integer, Int)
-----------------------------}

{-----------------------------
instance RealFloat Float where
    floatRadix  _ = primFloatRadix
    floatDigits _ = primFloatDigits
    floatRange  _ = (primFloatMinExp, primFloatMaxExp)
    encodeFloat = primFloatEncode
    decodeFloat = primFloatDecode
    isNaN       _ = False
    isInfinite  _ = False
    isDenormalized _ = False
    isNegativeZero _ = False
    isIEEE      _ = False
-----------------------------}

{-----------------------------
primitive primDoubleRadix  :: Integer
primitive primDoubleDigits :: Int
primitive primDoubleMinExp,
          primDoubleMaxExp :: Int
primitive primDoubleEncode :: Integer -> Int -> Double
primitive primDoubleDecode :: Double -> (Integer, Int)
-----------------------------}

{-----------------------------
instance RealFloat Double where
    floatRadix  _ = primDoubleRadix
    floatDigits _ = primDoubleDigits
    floatRange  _ = (primDoubleMinExp, primDoubleMaxExp)
    encodeFloat   = primDoubleEncode
    decodeFloat   = primDoubleDecode
    isNaN       _ = False
    isInfinite  _ = False
    isDenormalized _ = False
    isNegativeZero _ = False
    isIEEE      _ = False

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
-----------------------------}

{-----------------------------
primitive primShowsFloat :: Int -> Float -> ShowS
-----------------------------}

{-----------------------------
instance Read Float where
    readsPrec p = readSigned readFloat

-- Note that showFloat in Numeric isn't used here
instance Show Float where
    showsPrec   = primShowsFloat
-----------------------------}

{-----------------------------
primitive primShowsDouble :: Int -> Double -> ShowS
-----------------------------}

{-----------------------------
instance Read Double where
    readsPrec p = readSigned readFloat

-- Note that showFloat in Numeric isn't used here
instance Show Double where
    showsPrec   = primShowsDouble
-----------------------------}

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

asTypeOf       :: a -> a -> a
asTypeOf        = const

{-----------------------------
error          :: String -> a
error s         = throw (ErrorCall s)

undefined      :: a
undefined       = error "Prelude.undefined"
-----------------------------}
error :: String -> a
error s = throw (ErrorCall s)

undefined :: forall a . a
undefined = error "Prelude.undefined"

-- Standard functions on rational numbers {PreludeRatio} --------------------

{-----------------------------
data Integral a => Ratio a = !a :% !a deriving (Eq)
-----------------------------}
data Ratio a = !a :% !a deriving (Eq)
type Rational              = Ratio Integer

{-----------------------------
-----------------------------}
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

{-----------------------------
-----------------------------}
instance Integral a => Num (Ratio a) where
    (x:%y) + (x':%y') = reduce (x*y' + x'*y) (y*y')
    (x:%y) * (x':%y') = reduce (x*x') (y*y')
    negate (x :% y)   = negate x :% y
    abs (x :% y)      = abs x :% y
    signum (x :% y)   = signum x :% 1
    fromInteger x     = fromInteger x :% 1
    fromInt           = intToRatio

{-----------------------------
-----------------------------}
-- Hugs optimises code of the form fromRational (intToRatio x)
intToRatio :: Integral a => Int -> Ratio a
intToRatio x = fromInt x :% 1

{-----------------------------
instance Integral a => Real (Ratio a) where
    toRational (x:%y) = toInteger x :% toInteger y
-----------------------------}

instance Integral a => Fractional (Ratio a) where
    (x:%y) / (x':%y')   = (x*y') % (y*x')
    recip (x:%y)        = y % x
    fromRational (x:%y) = fromInteger x :% fromInteger y
{-----------------------------
    fromDouble          = doubleToRatio
-----------------------------}

{-----------------------------
-- Hugs optimises code of the form fromRational (doubleToRatio x)
-- Since this function is private, and only used to convert floating point
-- literals, it yields a decimal fraction, hopefully the one the user
-- specified in the first place (but some precision may be lost).  A real
-- Haskell implementation would use Rational to represent these literals.
doubleToRatio :: Integral a => Double -> Ratio a
doubleToRatio x
            | n>=0      = (round (x / fromInteger pow) * fromInteger pow) % 1
            | otherwise = fromRational (round (x * fromInteger denom) % denom)
                          where (m,n) = decodeFloat x
                                n_dec = ceiling (logBase 10 (encodeFloat 1 n))
                                denom = 10 ^ (-n_dec)
                                pow   = 10 ^ n_dec
-----------------------------}

{-----------------------------
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

instance Integral a => Show (Ratio a) where
    showsPrec p (x:%y) = showParen (p > 7)
                             (showsPrec 8 x . showString " % " . showsPrec 8 y)
-----------------------------}

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

length           :: [a] -> Int
length            = foldl' (\n _ -> n + 1) 0

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
foldl' f a (x:xs) = (foldl' f $! f a x) xs

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
concatMap f       = concat . map f

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

-- PreludeText ----------------------------------------------------------------

{-----------------------------
reads        :: Read a => ReadS a
reads         = readsPrec 0
-----------------------------}

shows        :: Show a => a -> ShowS
shows         = showsPrec 0

{-----------------------------
read         :: Read a => String -> a
read s        =  case [x | (x,t) <- reads s, ("","") <- lex t] of
                      [x] -> x
                      []  -> error "Prelude.read: no parse"
                      _   -> error "Prelude.read: ambiguous parse"
-----------------------------}

showChar     :: Char -> ShowS
showChar      = (:)

showString   :: String -> ShowS
showString    = (++)

showParen    :: Bool -> ShowS -> ShowS
showParen b p = if b then showChar '(' . p . showChar ')' else p

{-----------------------------
showField    :: Show a => String -> a -> ShowS
showField m@(c:_) v
  | isAlpha c || c == '_' = showString m . showString " = " . shows v
  | otherwise = showChar '(' . showString m . showString ") = " . shows v
-----------------------------}

{-----------------------------
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
                                               ch /= "'"                ]
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
-----------------------------}

isOctDigit c  =  c >= '0' && c <= '7'
isHexDigit c  =  isDigit c || c >= 'A' && c <= 'F'
                           || c >= 'a' && c <= 'f'

{-----------------------------
lexmatch                   :: (Eq a) => [a] -> [a] -> ([a],[a])
lexmatch (x:xs) (y:ys) | x == y  =  lexmatch xs ys
lexmatch xs     ys               =  (xs,ys)
-----------------------------}

asciiTab = zip ['\NUL'..' ']
           ["NUL", "SOH", "STX", "ETX", "EOT", "ENQ", "ACK", "BEL",
            "BS",  "HT",  "LF",  "VT",  "FF",  "CR",  "SO",  "SI",
            "DLE", "DC1", "DC2", "DC3", "DC4", "NAK", "SYN", "ETB",
            "CAN", "EM",  "SUB", "ESC", "FS",  "GS",  "RS",  "US",
            "SP"]

{-----------------------------
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
-----------------------------}

{-----------------------------
-----------------------------}
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

{-----------------------------
-- Unsigned readers for various bases
readDec, readOct, readHex :: Integral a => ReadS a
readDec = readInt 10 isDigit    (\ d -> fromEnum d - fromEnum_0)
readOct = readInt  8 isOctDigit (\ d -> fromEnum d - fromEnum_0)
readHex = readInt 16 isHexDigit hex
            where hex d = fromEnum d - (if isDigit d then fromEnum_0
                                       else fromEnum (if isUpper d then 'A' else 'a') - 10)

fromEnum_0 :: Int
fromEnum_0 = fromEnum '0'
-----------------------------}

{-----------------------------
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
                       readExp s                     = [(0,s)]

                       readExp' ('-':s) = [(-k,t) | (k,t) <- readDec s]
                       readExp' ('+':s) = readDec s
                       readExp' s       = readDec s
-----------------------------}

----------------------------------------------------------------
-- Exception datatype and operations
----------------------------------------------------------------

data Exception
  = ArithException      ArithException
  | ArrayException      ArrayException
  | AssertionFailed     String
  | AsyncException      AsyncException
  | BlockedOnDeadMVar
  | Deadlock
{-----------------------------
  | DynException        Dynamic
-----------------------------}
  | ErrorCall           String
  | ExitException       ExitCode
  | IOException         IOException     -- IO exceptions (from 'ioError')
  | NoMethodError       String
  | NonTermination
  | PatternMatchFail    String
  | RecConError         String
  | RecSelError         String
  | RecUpdError         String

instance Show Exception where
  showsPrec _ (ArithException e)  = shows e
  showsPrec _ (ArrayException e)  = shows e
  showsPrec _ (AssertionFailed s) = showException "assertion failed" s
  showsPrec _ (AsyncException e)  = shows e
  showsPrec _ BlockedOnDeadMVar   = showString "thread blocked indefinitely"
  showsPrec _ Deadlock            = showString "<<deadlock>>"
{-----------------------------
  showsPrec _ (DynException _)    = showString "unknown exception"
-----------------------------}
  showsPrec _ (ErrorCall s)       = showString s
  showsPrec _ (ExitException err) = showString "exit: " . shows err
  showsPrec _ (IOException err)   = shows err
  showsPrec _ (NoMethodError s)   = showException "undefined member" s
  showsPrec _ NonTermination      = showString "<<loop>>"
  showsPrec _ (PatternMatchFail s) = showException "pattern match failure" s
  showsPrec _ (RecConError s)     = showException "undefined field" s
  showsPrec _ (RecSelError s)     = showException "select of missing field" s
  showsPrec _ (RecUpdError s)     = showException "update of missing field" s

data ArithException
  = Overflow
  | Underflow
  | LossOfPrecision
  | DivideByZero
  | Denormal
  deriving (Eq, Ord)

instance Show ArithException where
  showsPrec _ Overflow        = showString "arithmetic overflow"
  showsPrec _ Underflow       = showString "arithmetic underflow"
  showsPrec _ LossOfPrecision = showString "loss of precision"
  showsPrec _ DivideByZero    = showString "divide by zero"
  showsPrec _ Denormal        = showString "denormal"

data ArrayException
  = IndexOutOfBounds    String
  | UndefinedElement    String
  deriving (Eq, Ord)

instance Show ArrayException where
  showsPrec _ (IndexOutOfBounds s) =
    showException "array index out of range" s
  showsPrec _ (UndefinedElement s) =
    showException "undefined array element" s

data AsyncException
  = StackOverflow
  | HeapOverflow
{-----------------------------
  | ThreadKilled
-----------------------------}
  deriving (Eq, Ord)

instance Show AsyncException where
  showsPrec _ StackOverflow   = showString "stack overflow"
  showsPrec _ HeapOverflow    = showString "heap overflow"
{-----------------------------
  showsPrec _ ThreadKilled    = showString "thread killed"
-----------------------------}

showException :: String -> String -> ShowS
showException tag msg =
  showString tag . (if null msg then id else showString ": " . showString msg)

data ExitCode = ExitSuccess | ExitFailure Int
{-----------------------------
                deriving (Eq, Ord, Read, Show)
-----------------------------}
                deriving (Eq, Ord, Show)

-- data type describing IOErrors / exceptions.
type IOError = IOException

data IOException
  = IOError
      { ioe_handle      :: Maybe Handle   -- the handle used by the action
                                          -- flagging the error
      , ioe_type        :: IOErrorType    -- what kind of (std) error
      , ioe_location    :: String         -- location of the error
      , ioe_description :: String         -- error-specific string
      , ioe_filename    :: Maybe FilePath -- the resource involved.
      } 
      deriving (Eq)

data IOErrorType
  = AlreadyExists
  | NoSuchThing
  | ResourceBusy
  | ResourceExhausted
  | EOF
  | IllegalOperation
  | PermissionDenied
  | UserError
     -- GHC compatibility
  | ProtocolError
  | UnsupportedOperation
  | OtherError
     -- DOTNET only
  | DotNetException
    deriving (Eq)

instance Show IOErrorType where
  show x = 
    case x of
      AlreadyExists     -> "already exists"
      NoSuchThing       -> "does not exist"
      ResourceBusy      -> "resource busy"
      ResourceExhausted -> "resource exhausted"
      EOF               -> "end of file"
      IllegalOperation  -> "illegal operation"
      PermissionDenied  -> "permission denied"
      UserError         -> "user error"
      ProtocolError     -> "protocol error"
      UnsupportedOperation -> "unsupported operation"
      OtherError        -> "failed"
      DotNetException   -> ".NET exception"

instance Show IOException where
  showsPrec p (IOError hdl iot loc s fn) =
    (case fn of
       Nothing -> case hdl of
                      Nothing -> id
                      Just h  -> showsPrec p h . showString ": "
       Just name -> showString name . showString ": ") .
    (case loc of
       "" -> id
       _  -> showString loc . showString ": ") .
    showsPrec p iot .
    (case s of
       "" -> id
       _  -> showString " (" . showString s . showString ")")

-- Monadic I/O: --------------------------------------------------------------

--data IO a             -- builtin datatype of IO actions

type FilePath = String  -- file pathnames are represented by strings

{-----------------------------
primitive primbindIO             :: IO a -> (a -> IO b) -> IO b
primitive primretIO              :: a -> IO a
-----------------------------}

ioFromPrim :: (() -> a) -> IO a
ioFromPrim f
  = IO (\_ -> letstrict x = f () in IOResult x)

primbindIO :: IO a -> (a -> IO b) -> IO b
primbindIO (IO io) f
  = IO (\_ -> case io () of
                IOResult x
                  -> letstrict x' = x
                     in       case f x' of
                                IO fx -> fx ()
       )

primretIO :: a -> IO a
primretIO x
  = IO (\_ -> IOResult x)

{-----------------------------
ioError :: IOError -> IO a
ioError e = IO (\ s -> throw (IOException e))

userError :: String -> IOError
userError str = IOError Nothing UserError "" str Nothing
-----------------------------}

{-----------------------------
catch :: IO a -> (IOError -> IO a) -> IO a
catch m h = catchException m $ \e -> case e of
                IOException err -> h err
                _ -> throw e
-----------------------------}
foreign import ccall primCatchException :: forall exc a . a -> (exc -> a) -> a

catch :: IO a -> (IOError -> IO a) -> IO a
catch m h = primCatchException m $ \e -> case e of
                IOException err -> h err
                _ -> throw e

{-----------------------------
putChar   :: Char -> IO ()
putChar    = hPutChar stdout

putStr    :: String -> IO ()
putStr     = hPutStr stdout
-----------------------------}

print     :: Show a => a -> IO ()
print      = putStrLn . show

{-----------------------------
putStrLn  :: String -> IO ()
putStrLn s = do putStr s
                putChar '\n'

getChar   :: IO Char
getChar    = hGetChar stdin

getContents :: IO String
getContents  = hGetContents stdin

getLine   :: IO String
getLine    = hGetLine stdin

hGetLine :: Handle -> IO String
hGetLine h = do 
  c <- hGetChar h
  hGetLine' c
  where
   hGetLine' '\n' = return ""
   hGetLine' c = do
     cs <- getRest
     return (c:cs)
   getRest = do
     c <- catch (hGetChar h) $ \ ex ->
        if isEOFError ex then return '\n' else ioError ex
     hGetLine' c
   isEOFError ex = ioe_type ex == EOF   -- defined in System.IO.Error

-- raises an exception instead of an error
readIO          :: Read a => String -> IO a
readIO s         = case [x | (x,t) <- reads s, ("","") <- lex t] of
                        [x] -> return x
                        []  -> ioError (userError "PreludeIO.readIO: no parse")
                        _   -> ioError (userError 
                                       "PreludeIO.readIO: ambiguous parse")

readLn          :: Read a => IO a
readLn           = do l <- getLine
                      r <- readIO l
                      return r

data IOMode      =  ReadMode | WriteMode | AppendMode | ReadWriteMode
                    deriving (Eq, Ord, Ix, Bounded, Enum, Read, Show)

writeFile       :: FilePath -> String -> IO ()
writeFile        = writeFile' WriteMode

appendFile      :: FilePath -> String -> IO ()
appendFile       = writeFile' AppendMode

writeFile'      :: IOMode -> FilePath -> String -> IO ()
writeFile' mode name s = do
  h <- openFile name mode
  catchException (hPutStr h s) (\e -> hClose h >> throw e)
  hClose h

readFile        :: FilePath -> IO String
readFile name    = openFile name ReadMode >>= hGetContents

interact  :: (String -> String) -> IO ()
interact f = getContents >>= (putStr . f)
-----------------------------}

{-----------------------------
primitive stdin       :: Handle
primitive stdout      :: Handle
primitive stderr      :: Handle
-----------------------------}
foreign import ccall "primStdin"  stdin  :: Handle
foreign import ccall "primStdout" stdout :: Handle
foreign import ccall "primStderr" stderr :: Handle

{-----------------------------
primitive openFile    :: FilePath -> IOMode -> IO Handle
primitive hClose      :: Handle -> IO ()
primitive hGetContents :: Handle -> IO String
primitive hGetChar    :: Handle -> IO Char
primitive hPutChar    :: Handle -> Char -> IO ()
primitive hPutStr     :: Handle -> String -> IO ()
-----------------------------}
foreign import ccall primWriteChan :: Handle -> ByteArray -> ()
foreign import ccall primFlushChan :: Handle -> ()

hPutStr, hPutStrLn     :: Handle -> String -> IO ()
hPutStr   h s = ioFromPrim (\_ -> primWriteChan h (primStringToByteArray s 1000))
hPutStrLn h s = do {hPutStr h s ; hPutStr h "\n"}

hFlush     :: Handle -> IO ()
hFlush h = ioFromPrim (\_ -> primFlushChan h)

putStr, putStrLn     :: String -> IO ()
putStr   = hPutStr   stdout
putStrLn = hPutStrLn stdout

{-----------------------------
instance Functor IO where
    fmap f x = x >>= (return . f)
-----------------------------}

instance Monad IO where
    (>>=)  = primbindIO
    return = primretIO
    
{-----------------------------
    fail s = ioError (userError s)
-----------------------------}

-- PackedString -----------------------------------------------------

data PackedString

foreign import ccall "primCStringToString"  packedStringToString  :: PackedString -> [Char]
foreign import ccall "primCStringToInteger" packedStringToInteger :: PackedString -> Integer

-- ByteArray -----------------------------------------------------

data ByteArray

foreign import ccall primByteArrayLength   :: ByteArray -> Int
foreign import ccall primByteArrayToString :: ByteArray -> String
foreign import ccall primStringToByteArray :: String -> Int -> ByteArray

-- Hooks for primitives: -----------------------------------------------------
-- Do not mess with these!

data FunPtr a -- builtin datatype of C function pointers
data Ptr a    -- builtin datatype of C pointers
data Addr     -- builtin datatype of C pointers (deprecated)
data Word     -- builtin datatype of unsigned ints (deprecated)
data Int8
data Int16
data Int32
data Int64
data Word8
data Word16
data Word32
data Word64
data ForeignObj  -- builtin datatype of C pointers with finalizers (deprecated)
data ForeignPtr a -- builtin datatype of C pointers with finalizers
data StablePtr a
data Handle

data Object a -- builtin datatype of external object references.
              -- (needed as primitive since they're supported in FFI decls.)

instance Eq Handle where (==) = primEqHandle
{-----------------------------
primitive primEqHandle :: Handle -> Handle -> Bool
-----------------------------}
foreign import ccall "primEqChan" primEqHandle :: Handle -> Handle -> Bool

{-----------------------------
instance Show Handle where
    showsPrec _ h = case primGetHandleNumber h of
        0 -> showString "<stdin>"
        1 -> showString "<stdout>"
        2 -> showString "<stderr>"
        _ -> showString "<handle>"
-----------------------------}
instance Show Handle where
    showsPrec _ h
      = if      n == 0 then showString "<stdin>"
        else if n == 1 then showString "<stdout>"
        else if n == 2 then showString "<stderr>"
        else                showString "<handle>"
      where n = primGetHandleNumber h

{-----------------------------
primitive primGetHandleNumber :: Handle -> Int

primitive unsafeCoerce "primUnsafeCoerce" :: a -> b
-----------------------------}
foreign import ccall "primChanNumber" primGetHandleNumber :: Handle -> Int
foreign import ccall "primUnsafeId" unsafeCoerce :: a -> b

{-----------------------------
data Dynamic = Dynamic TypeRep Obj

data TypeRep = TypeRep !Key TyCon [TypeRep]

instance Eq TypeRep where
  (TypeRep k1 _ _) == (TypeRep k2 _ _) = k1 == k2

data TyCon = TyCon !Key String

instance Eq TyCon where
  (TyCon t1 _) == (TyCon t2 _) = t1 == t2

newtype Key = Key Int deriving( Eq )

data Obj = Obj

toObj :: a -> Obj
toObj   = unsafeCoerce

fromObj :: Obj -> a
fromObj = unsafeCoerce
-----------------------------}

{-----------------------------
newtype IO a = IO ((a -> IOResult) -> IOResult)
-----------------------------}
newtype IO a = IO (() -> IOResult a)

{-----------------------------
data IOResult 
  = Hugs_ExitWith    Int
  | Hugs_Catch       IOResult (Exception -> IOResult) (Obj -> IOResult)
  | Hugs_ForkThread  IOResult IOResult
  | Hugs_DeadThread
  | Hugs_YieldThread IOResult
  | Hugs_Return      Obj
  | Hugs_BlockThread (Obj -> IOResult) ((Obj -> IOResult) -> IOResult) 
-----------------------------}
newtype IOResult a = IOResult a

{-----------------------------
data IOFinished a
  = Finished_ExitWith Int
  | Finished_Return   a
-----------------------------}

{-----------------------------
primitive throw "primThrowException" :: Exception -> a
primitive primCatchException :: a -> Either Exception a
-----------------------------}
foreign import ccall primThrowException :: forall exc a . exc -> a

throw :: Exception -> a
throw e = primThrowException e

{-----------------------------
catchException :: IO a -> (Exception -> IO a) -> IO a
catchException (IO m) k = IO $ \ s ->
  Hugs_Catch (m hugsReturn)
             (\ e -> case (k e) of { IO k' -> k' s })
             (s . fromObj)

hugsReturn :: a -> IOResult
hugsReturn x = Hugs_Return (toObj x)

-- reify current thread, execute 'm <thread>' and switch to next thread
blockIO :: ((a -> IOResult) -> IO ()) -> IO a
blockIO m = IO (\ s -> Hugs_BlockThread (s . fromObj) m')
 where
  m' k = threadToIOResult (m (k . toObj))

hugsIORun  :: IO a -> Either Int a
hugsIORun m = 
  case basicIORun (runAndShowError m) of
    Finished_ExitWith i -> Left i
    Finished_Return   a -> Right a
 where
  runAndShowError :: IO a -> IO a
  runAndShowError m = m `catchException` exceptionHandler
  exceptionHandler :: Exception -> IO a
  exceptionHandler (ExitException ExitSuccess) = primExitWith 0
  exceptionHandler (ExitException (ExitFailure n)) = primExitWith n
  exceptionHandler err = runAndShowError $ do
        putChar '\n'
        putStr "Program error: "
        putStrLn (show err)
        primExitWith 1

basicIORun :: IO a -> IOFinished a
basicIORun (IO m) = loop [m hugsReturn]

threadToIOResult :: IO a -> IOResult
threadToIOResult (IO m) = m (const Hugs_DeadThread)

-- This is the queue of *runnable* threads.
-- There may be blocked threads attached to MVars
-- An important invariant is that at most one thread will result in
-- Hugs_Return - and its Obj value has type \alpha
loop :: [IOResult] -> IOFinished a
loop []                      = error "no more threads (deadlock?)"
loop [Hugs_Return   a]       = Finished_Return (fromObj a)
loop (Hugs_Return   a:r)     = loop (r ++ [Hugs_Return a])
loop (Hugs_Catch m f s:r)    = loop (hugs_catch m f s : r)
loop (Hugs_ExitWith i:_)     = Finished_ExitWith i
loop (Hugs_DeadThread:r)     = loop r
loop (Hugs_ForkThread a b:r) = loop (a:b:r)
loop (Hugs_YieldThread a:r)  = loop (r ++ [a])
loop (Hugs_BlockThread a b:r)= loop (b a : r)
loop _                       = error "Fatal error in Hugs scheduler"

hugs_catch :: IOResult -> (Exception -> IOResult) -> (Obj -> IOResult) -> IOResult
hugs_catch m f s = case primCatchException (catch' m) of
  Left  exn                   -> f exn
  Right (Hugs_Return a)       -> s a
  Right (Hugs_ForkThread a b) -> Hugs_ForkThread (Hugs_Catch a f s) b
  Right (Hugs_YieldThread a)  -> Hugs_YieldThread (Hugs_Catch a f s)
  Right (Hugs_BlockThread a b)-> Hugs_BlockThread (\x -> Hugs_Catch (a x) f s) b
  Right r                     -> r
 where
  catch' :: IOResult -> IOResult
  catch' (Hugs_Catch m' f' s') = catch' (hugs_catch m' f' s')
  catch' x                     = x
-----------------------------}

{-----------------------------
primExitWith     :: Int -> IO a
primExitWith c    = IO (\ s -> Hugs_ExitWith c)
-----------------------------}

primCompAux      :: Ord a => a -> a -> Ordering -> Ordering
primCompAux x y o = case compare x y of EQ -> o; LT -> LT; GT -> GT

{-----------------------------
primPmInt        :: Num a => Int -> a -> Bool
primPmInt n x     = fromInt n == x

primPmInteger    :: Num a => Integer -> a -> Bool
primPmInteger n x = fromInteger n == x

primPmFlt        :: Fractional a => Double -> a -> Bool
primPmFlt n x     = fromDouble n == x

-- The following primitives are only needed if (n+k) patterns are enabled:
primPmNpk        :: Integral a => Int -> a -> Maybe a
primPmNpk n x     = if n'<=x then Just (x-n') else Nothing
                    where n' = fromInt n

primPmSub        :: Integral a => Int -> a -> a
primPmSub n x     = x - fromInt n

-- Trex
emptyRec :: Rec EmptyRow
emptyRec = EmptyRec
-----------------------------}

-- End of Hugs standard prelude ----------------------------------------------

-- main = 3

