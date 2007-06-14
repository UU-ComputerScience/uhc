{----------------------------------------------------------------------------
The EHC prelude is copied/adapted from the Hugs prelude.
This variant is meant for testing only, compile standalone with: ehc --no-prelude
20070614: mem usage of AG, visit functions
----------------------------------------------------------------------------}

module TryPrel where

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

class Bounded a where
    minBound, maxBound :: a
    -- Minimal complete definition: All

-- Numeric classes ----------------------------------------------------------

class (Eq a) => Num a where
    (+), (-), (*)  :: a -> a -> a
    negate         :: a -> a
    abs, signum    :: a -> a
    fromInteger    :: Integer -> a
    fromInt        :: Int -> a

class Integral a where
    quot, rem, div, mod :: a -> a -> a
    quotRem, divMod     :: a -> a -> (a,a)
    toInteger           :: a -> Integer
    toInt               :: a -> Int

-- Index and Enumeration classes --------------------------------------------

class Enum a where
    succ, pred           :: a -> a
    toEnum               :: Int -> a
    fromEnum             :: a -> Int
    enumFrom             :: a -> [a]              -- [n..]
    enumFromThen         :: a -> a -> [a]         -- [n,m..]
    enumFromTo           :: a -> a -> [a]         -- [n..m]
    enumFromThenTo       :: a -> a -> a -> [a]    -- [n,n'..m]

-- Read and Show classes ------------------------------------------------------

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

seq :: a -> b -> b -- forall a . a -> forall b . b -> b
x `seq` y = letstrict x' = x in y

f $! x                = x `seq` f x

-- Trivial type -------------------------------------------------------------

-- data () = () deriving (Eq, Ord, Ix, Enum, Read, Show, Bounded)

-- Boolean type -------------------------------------------------------------

data Bool    = False | True

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

type String = [Char]    -- strings are lists of characters

foreign import ccall "primEqInt"   primEqChar    :: Char -> Char -> Bool
foreign import ccall "primCmpInt"  primCmpChar   :: Char -> Char -> Ordering

instance Eq Char  where (==)    = primEqChar
instance Ord Char where compare = primCmpChar

foreign import ccall "primUnsafeId"  primCharToInt   :: Char -> Int
foreign import ccall "primUnsafeId"  primIntToChar   :: Int -> Char

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

isSpace, isDigit :: Char -> Bool

isSpace c              =  c == ' '  ||
                          c == '\t' ||
                          c == '\n' ||
                          c == '\r' ||
                          c == '\f' ||
                          c == '\v' ||
                          c == '\xa0'

isDigit c              =  c >= '0'   &&  c <= '9'

foreign import ccall "primCharIsUpper"   isUpper    :: Char -> Bool
foreign import ccall "primCharIsLower"   isLower    :: Char -> Bool

isAlpha c = isUpper c || isLower c
isAlphaNum c = isAlpha c || isDigit c

-- Maybe type ---------------------------------------------------------------

data Maybe a = Nothing | Just a

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

either              :: (a -> c) -> (b -> c) -> Either a b -> c
either l r (Left x)  = l x
either l r (Right y) = r y

-- Ordering type ------------------------------------------------------------

data Ordering = LT | EQ | GT
                deriving (Eq)

-- Lists --------------------------------------------------------------------

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

instance Show a => Show [a]  where
    showsPrec p = showList

-- Tuples -------------------------------------------------------------------

-- data (a,b) = (a,b) deriving (Eq, Ord, Ix, Read, Show)
-- etc..

-- Standard Integral types --------------------------------------------------

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

foreign import ccall primAddInt       :: Int -> Int -> Int
foreign import ccall primSubInt       :: Int -> Int -> Int
foreign import ccall primMulInt       :: Int -> Int -> Int
foreign import ccall primNegInt       :: Int -> Int
foreign import ccall primIntegerToInt :: Integer -> Int

instance Num Int where
    (+)           = primAddInt
    (-)           = primSubInt
    negate        = primNegInt
    (*)           = primMulInt
--    abs           = absReal
--    signum        = signumReal
    fromInteger   = primIntegerToInt
    fromInt x     = x

foreign import ccall primMaxInt :: Int
foreign import ccall primMinInt :: Int

instance Bounded Int where
    minBound = primMinInt
    maxBound = primMaxInt

foreign import ccall primAddInteger       :: Integer -> Integer -> Integer
foreign import ccall primSubInteger       :: Integer -> Integer -> Integer
foreign import ccall primMulInteger       :: Integer -> Integer -> Integer
foreign import ccall primNegInteger       :: Integer -> Integer
foreign import ccall primIntToInteger     :: Int -> Integer

instance Num Integer where
    (+)           = primAddInteger
    (-)           = primSubInteger
    negate        = primNegInteger
    (*)           = primMulInteger
--    abs           = absReal
--    signum        = signumReal
    fromInteger x = x
    fromInt       = primIntToInteger

foreign import ccall primDivInt       :: Int -> Int -> Int
foreign import ccall primModInt       :: Int -> Int -> Int
foreign import ccall primQuotInt      :: Int -> Int -> Int
foreign import ccall primRemInt       :: Int -> Int -> Int

instance Integral Int where
    div       = primDivInt
    quot      = primQuotInt
    rem       = primRemInt
    mod       = primModInt
    toInteger = primIntToInteger
    toInt x   = x

foreign import ccall primQuotInteger          :: Integer -> Integer -> Integer
foreign import ccall primRemInteger           :: Integer -> Integer -> Integer
foreign import ccall primQuotRemInteger       :: Integer -> Integer -> (Integer,Integer)
foreign import ccall primDivInteger           :: Integer -> Integer -> Integer
foreign import ccall primModInteger           :: Integer -> Integer -> Integer
foreign import ccall primDivModInteger        :: Integer -> Integer -> (Integer,Integer)

instance Integral Integer where
    toInteger x = x
    toInt       = primIntegerToInt



-- takeWhile and one more
takeWhile1 :: (a -> Bool) -> [a] -> [a]
takeWhile1 p (x:xs) = x : if p x then takeWhile1 p xs else []


foreign import ccall primShowInt :: Int -> String


instance Show Int where
    show   = primShowInt



-- Standard Floating types --------------------------------------------------

data Float     -- builtin datatype of single precision floating point numbers
data Double    -- builtin datatype of double precision floating point numbers

foreign import ccall primEqFloat   :: Float -> Float -> Bool
foreign import ccall primCmpFloat  :: Float -> Float -> Ordering
foreign import ccall primEqDouble  :: Double -> Double -> Bool
foreign import ccall primCmpDouble :: Double -> Double -> Ordering

instance Eq  Float  where (==) = primEqFloat
instance Eq  Double where (==) = primEqDouble

instance Ord Float  where compare = primCmpFloat
instance Ord Double where compare = primCmpDouble

foreign import ccall primAddFloat       :: Float -> Float -> Float
foreign import ccall primSubFloat       :: Float -> Float -> Float
foreign import ccall primMulFloat       :: Float -> Float -> Float
foreign import ccall primNegFloat       :: Float -> Float
foreign import ccall primIntToFloat     :: Int -> Float
foreign import ccall primIntegerToFloat :: Integer -> Float

instance Num Float where
    (+)           = primAddFloat
    (-)           = primSubFloat
    negate        = primNegFloat
    (*)           = primMulFloat
--    abs           = absReal
--    signum        = signumReal
    fromInteger   = primIntegerToFloat
    fromInt       = primIntToFloat

foreign import ccall primAddDouble       :: Double -> Double -> Double
foreign import ccall primSubDouble       :: Double -> Double -> Double
foreign import ccall primMulDouble       :: Double -> Double -> Double
foreign import ccall primNegDouble       :: Double -> Double
foreign import ccall primIntToDouble     :: Int -> Double
foreign import ccall primIntegerToDouble :: Integer -> Double

instance Num Double where
    (+)         = primAddDouble
    (-)         = primSubDouble
    negate      = primNegDouble
    (*)         = primMulDouble
--    abs         = absReal
--    signum      = signumReal
    fromInteger = primIntegerToDouble
    fromInt     = primIntToDouble

foreign import ccall primDivFloat      :: Float -> Float -> Float
foreign import ccall primDoubleToFloat :: Double -> Float
-- foreign import ccall primFloatToDouble :: Float -> Double  -- used by runtime optimizer


foreign import ccall primDivDouble :: Double -> Double -> Double

foreign import ccall primRationalToFloat  :: Rational -> Float
foreign import ccall primRationalToDouble :: Rational -> Double

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

error :: String -> a
error s = throw (ErrorCall s)

undefined :: forall a . a
undefined = error "Prelude.undefined"

-- Standard functions on rational numbers {PreludeRatio} --------------------

data Ratio a = !a :% !a deriving (Eq)
type Rational              = Ratio Integer



numerator, denominator    :: Integral a => Ratio a -> a
numerator (x :% y)         = x
denominator (x :% y)       = y







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


shows        :: Show a => a -> ShowS
shows         = showsPrec 0


showChar     :: Char -> ShowS
showChar      = (:)

showString   :: String -> ShowS
showString    = (++)

showParen    :: Bool -> ShowS -> ShowS
showParen b p = if b then showChar '(' . p . showChar ')' else p



isOctDigit c  =  c >= '0' && c <= '7'
isHexDigit c  =  isDigit c || c >= 'A' && c <= 'F'
                           || c >= 'a' && c <= 'f'


asciiTab = zip ['\NUL'..' ']
           ["NUL", "SOH", "STX", "ETX", "EOT", "ENQ", "ACK", "BEL",
            "BS",  "HT",  "LF",  "VT",  "FF",  "CR",  "SO",  "SI",
            "DLE", "DC1", "DC2", "DC3", "DC4", "NAK", "SYN", "ETB",
            "CAN", "EM",  "SUB", "ESC", "FS",  "GS",  "RS",  "US",
            "SP"]


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



----------------------------------------------------------------
-- Exception datatype and operations
----------------------------------------------------------------

data Exception
  = ErrorCall           String

instance Show Exception where
  showsPrec _ (ErrorCall s)       = showString s





showException :: String -> String -> ShowS
showException tag msg =
  showString tag . (if null msg then id else showString ": " . showString msg)

data ExitCode = ExitSuccess | ExitFailure Int

-- data type describing IOErrors / exceptions.

-- Monadic I/O: --------------------------------------------------------------

--data IO a             -- builtin datatype of IO actions

type FilePath = String  -- file pathnames are represented by strings


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




print     :: Show a => a -> IO ()
print      = putStrLn . show


foreign import ccall "primStdin"  stdin  :: Handle
foreign import ccall "primStdout" stdout :: Handle
foreign import ccall "primStderr" stderr :: Handle

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


instance Monad IO where
    (>>=)  = primbindIO
    return = primretIO
    

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
foreign import ccall "primEqChan" primEqHandle :: Handle -> Handle -> Bool

instance Show Handle where
    showsPrec _ h
      = if      n == 0 then showString "<stdin>"
        else if n == 1 then showString "<stdout>"
        else if n == 2 then showString "<stderr>"
        else                showString "<handle>"
      where n = primGetHandleNumber h

foreign import ccall "primChanNumber" primGetHandleNumber :: Handle -> Int
foreign import ccall "primUnsafeId" unsafeCoerce :: a -> b


newtype IO a = IO (() -> IOResult a)

newtype IOResult a = IOResult a


foreign import ccall primThrowException :: forall exc a . exc -> a

throw :: Exception -> a
throw e = primThrowException e


primCompAux      :: Ord a => a -> a -> Ordering -> Ordering
primCompAux x y o = case compare x y of EQ -> o; LT -> LT; GT -> GT


-- End of Hugs standard prelude ----------------------------------------------

main = putStrLn "Hello World!"

