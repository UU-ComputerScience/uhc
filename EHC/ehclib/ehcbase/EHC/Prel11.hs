

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

-- Ordering type ------------------------------------------------------------

data Ordering = LT | EQ | GT

-- Lists --------------------------------------------------------------------

data [] a = ''[]'' | a : [a]

-- Standard Int types --------------------------------------------------

foreign import ccall primGtInt      :: Int -> Int -> Bool
foreign import ccall primLtInt      :: Int -> Int -> Bool
foreign import ccall primEqInt      :: Int -> Int -> Bool
foreign import ccall primCmpInt     :: Int -> Int -> Ordering

foreign import ccall primAddInt       :: Int -> Int -> Int
foreign import ccall primSubInt       :: Int -> Int -> Int
foreign import ccall primMulInt       :: Int -> Int -> Int
foreign import ccall primNegInt       :: Int -> Int
foreign import ccall primDivInt       :: Int -> Int -> Int
foreign import ccall primModInt       :: Int -> Int -> Int
foreign import ccall primQuotInt      :: Int -> Int -> Int
foreign import ccall primRemInt       :: Int -> Int -> Int


-- Evaluation and strictness ------------------------------------------------

seq :: a -> b -> b -- forall a . a -> forall b . b -> b
x `seq` y = letstrict x' = x in y

f $! x                = x `seq` f x

asTypeOf       :: a -> a -> a
asTypeOf        = const

data PackedString
foreign import ccall "primCStringToString" packedStringToString :: PackedString -> [Char]
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
xs     !! n | n<0 = error "Prelude.!!: negative index"
[]     !! _       = error "Prelude.!!: index too large"
(x:_)  !! 0       = x
(_:xs) !! n       = xs !! (n-1)

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
concatMap f       = concat . map f

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









--===========================================================================


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

-- Numeric classes ----------------------------------------------------------

class (Eq a) => Num a where
    (+), (-), (*)  :: a -> a -> a
    negate         :: a -> a
    abs, signum    :: a -> a
    fromInt        :: Int -> a
    -- fromInteger    :: Integer -> a



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


-- Lists --------------------------------------------------------------------


instance Eq a => Eq [a] where
    []     == []     =  True
    (x:xs) == (y:ys) =  x==y && xs==ys
    _      == _      =  False

instance Ord a => Ord [a] where
    compare []     (_:_)  = LT
    compare []     []     = EQ
    compare (_:_)  []     = GT
    compare (x:xs) (y:ys) = primCompAux x y (compare xs ys)

primCompAux      :: Ord a => a -> a -> Ordering -> Ordering
primCompAux x y o = case compare x y of EQ -> o; LT -> LT; GT -> GT

instance Eq  Int     where (==)    = primEqInt

instance Ord Int     where
  compare = primCmpInt
  (<)     = primLtInt
  (>)     = primGtInt

instance Num Int where
    (+)           = primAddInt
    (-)           = primSubInt
    negate        = primNegInt
    (*)           = primMulInt
--    abs           = absReal
--    signum        = signumReal
--    fromInteger   = primIntegerToInt
    fromInt x     = x


--=========================================================

-- Character type -----------------------------------------------------------

type String = [Char]    -- strings are lists of characters

foreign import ccall "primEqInt"   primEqChar    :: Char -> Char -> Bool
foreign import ccall "primCmpInt"  primCmpChar   :: Char -> Char -> Ordering

instance Eq Char  where (==)    = primEqChar
instance Ord Char where compare = primCmpChar

isSpace, isDigit :: Char -> Bool

isSpace c              =  c == ' '  ||
                          c == '\t' ||
                          c == '\n' ||
                          c == '\r' ||
                          c == '\f' ||
                          c == '\v' 
                          -- || c == '\xa0'

isDigit c              =  c >= '0'   &&  c <= '9'




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


--========================================================================================

class Bounded a where
    minBound, maxBound :: a

-- Enumeration classes --------------------------------------------

class Enum a where
    succ, pred           :: a -> a
    toEnum               :: Int -> a
    fromEnum             :: a -> Int
    enumFrom             :: a -> [a]              -- [n..]
    enumFromThen         :: a -> a -> [a]         -- [n,m..]
    enumFromTo           :: a -> a -> [a]         -- [n..m]
    enumFromThenTo       :: a -> a -> a -> [a]    -- [n,n'..m]

-- Monad classes ------------------------------------------------------------

class Functor f where
    fmap :: (a -> b) -> (f a -> f b)

class Monad m where
    return :: a -> m a
    (>>=)  :: m a -> (a -> m b) -> m b
    (>>)   :: m a -> m b -> m b
    fail   :: [Char] -> m a

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



-- Maybe type ---------------------------------------------------------------

instance Functor Maybe where
    fmap f Nothing  = Nothing
    fmap f (Just x) = Just (f x)

instance Monad Maybe where
    Just x  >>= k = k x
    Nothing >>= k = Nothing
    return        = Just
    fail s        = Nothing

instance Functor [] where
    fmap = map

instance Monad [ ] where
    (x:xs) >>= f = f x ++ (xs >>= f)
    []     >>= f = []
    return x     = [x]
    fail s       = []

foreign import ccall primMaxInt :: Int
foreign import ccall primMinInt :: Int

instance Bounded Int where
    minBound = primMinInt
    maxBound = primMaxInt




--=========================================================================
-- Show classes ------------------------------------------------------

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
    -- maxBound = '\xff' -- primMaxChar


instance Show a => Show [a]  where
    showsPrec p = showList




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

{-
asciiTab = zip ['\NUL'..' ']
           ["NUL", "SOH", "STX", "ETX", "EOT", "ENQ", "ACK", "BEL",
            "BS",  "HT",  "LF",  "VT",  "FF",  "CR",  "SO",  "SI",
            "DLE", "DC1", "DC2", "DC3", "DC4", "NAK", "SYN", "ETB",
            "CAN", "EM",  "SUB", "ESC", "FS",  "GS",  "RS",  "US",
            "SP"]
-}

showLitChar               :: Char -> ShowS
--showLitChar c | c > '\DEL' = showChar '\\' .
--                             protectEsc isDigit (shows (fromEnum c))
--showLitChar '\DEL'         = showString "\\DEL"
showLitChar '\\'           = showString "\\\\"
showLitChar c | c >= ' '   = showChar c
--showLitChar '\a'           = showString "\\a"
--showLitChar '\b'           = showString "\\b"
--showLitChar '\f'           = showString "\\f"
--showLitChar '\n'           = showString "\\n"
--showLitChar '\r'           = showString "\\r"
--showLitChar '\t'           = showString "\\t"
--showLitChar '\v'           = showString "\\v"
--showLitChar '\SO'          = protectEsc ('H'==) (showString "\\SO")
--showLitChar c              = showString ('\\' : snd (asciiTab!!fromEnum c))

protectEsc p f             = f . cont
 where cont s@(c:_) | p c  = "\\&" ++ s
       cont s              = s


foreign import ccall primShowInt :: Int -> String


instance Show Int where
    show   = primShowInt


data Exception
  = ErrorCall           String

instance Show Exception where
  showsPrec _ (ErrorCall s)       = showString s





showException :: String -> String -> ShowS
showException tag msg =
  showString tag . (if null msg then id else showString ": " . showString msg)

data ExitCode = ExitSuccess | ExitFailure Int
