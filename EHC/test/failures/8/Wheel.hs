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

otherwise   :: Bool
otherwise    = True

-- Lists --------------------------------------------------------------------

data [] a = ''[]'' | a : [a]

-- Evaluation and strictness ------------------------------------------------

error :: [Char] -> a
error s = undefined

undefined :: forall a . a
undefined = error "undefined"

-- Some standard functions --------------------------------------------------

const          :: a -> b -> a
const k _       = k

-- Standard list functions {PreludeList} ------------------------------------

head             :: [a] -> a
head (x:_)        = x

tail             :: [a] -> [a]
tail (_:xs)       = xs

(++)             :: [a] -> [a] -> [a]
[]     ++ ys      = ys
(x:xs) ++ ys      = x : (xs ++ ys)

map              :: (a -> b) -> [a] -> [b]
map f xs          = [ f x | x <- xs ]
--map f []          = []
--map f (x:xs)      = f x : map f xs

filter           :: (a -> Bool) -> [a] -> [a]
filter p xs     = [ x | x <- xs, p x ]
--filter p []       = []
--filter p (x:xs) | p x       = x : filter p xs
--                | otherwise =     filter p xs 

foldr            :: (a -> b -> b) -> b -> [a] -> b
foldr f z []      = z
foldr f z (x:xs)  = f x (foldr f z xs)

concat           :: [[a]] -> [a]
concat            = foldr (++) []

concatMap        :: (a -> [b]) -> [a] -> [b]
--concatMap f       = concat . map f
concatMap f []    = []
concatMap f (x:xs) = f x ++ concatMap f xs

zipWith                  :: (a->b->c) -> [a]->[b]->[c]
zipWith z (a:as) (b:bs)   = z a b : zipWith z as bs
zipWith _ _      _        = []

(!!)             :: [a] -> Int -> a
(x:xs) !! n       = if n == 0
                    then x
                    else xs !! (n-1)


--==========================PRIMITIVES==========================================
-- Primitive Int functions --------------------------------------------------

foreign import ccall primGtInt    :: Int -> Int -> Bool
foreign import ccall primLtInt    :: Int -> Int -> Bool
foreign import ccall primEqInt    :: Int -> Int -> Bool

foreign import ccall primAddInt   :: Int -> Int -> Int
foreign import ccall primSubInt   :: Int -> Int -> Int
foreign import ccall primMulInt   :: Int -> Int -> Int
foreign import ccall primModInt   :: Int -> Int -> Int

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
-- Eq (Specialised for Int)

(==)  =  primEqInt

-- Ord (Specialised for Int)

x <= y  =  x<y || x==y
(>)   =  primGtInt
(<)   =  primLtInt

-- Integral (Specialised for Int)

mod  = primModInt

-- Num (Specialised for Int)

(+)   =  primAddInt
(-)   =  primSubInt
(*)   =  primMulInt

-- Enum (Specialised for Int)


enumFromThenTo         :: Int -> Int -> Int -> [Int]
enumFromThenTo x y z 
  | x > z = []
  | otherwise          = x : enumFromThenTo y (y+(y-x)) z


--============================================================

{-
 - This is a EHC modified version of th primes benchmark of the nofib/imaginary
 - Original code by Colin Runciman (colin@cs.york.ac.uk)
 - Changes to be compiled by EHC: John van Schie
 -}

data Wheel = Wheel Int [Int]

primes :: [Int]
primes = sieve wheels primes squares

sieve (Wheel s ns:ws) ps qs =
  [n' | o <- s:[s*2,s*3..(head ps-1)*s],
        n <- ns,
        n'<- [n+o], noFactor n'] 
  ++
  sieve ws (tail ps) (tail qs)
  where
  noFactor = if s<=2 then const True else notDivBy ps qs

notDivBy (p:ps) (q:qs) n =
  q > n || n `mod` p > 0 && notDivBy ps qs n

squares :: [Int]
squares = [p*p | p<-primes]

wheels :: [Wheel]
wheels = Wheel 1 [1] : zipWith nextSize wheels primes 

nextSize (Wheel s ns) p =
  Wheel (s*p) ns'
  where
  ns' = [n' | o <- [0,s..(p-1)*s],
              n <- ns,
              n' <- [n+o], n'`mod`p > 0]

main =	primes !! 3
