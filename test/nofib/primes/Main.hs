{-
 - This is a EHC modified version of th primes benchmark of the nofib/imaginary
 - Original code by ???
 - Changes to be compiled by EHC: John van Schie
 -}
foreign import ccall "primAddInt" (+)   :: Int -> Int -> Int
foreign import ccall "primSubInt" (-)   :: Int -> Int -> Int
foreign import ccall "primModInt" mod   :: Int -> Int -> Int

foreign import ccall "primEqInt" (==)   :: Int -> Int -> Bool
foreign import ccall "primNeInt" (/=)   :: Int -> Int -> Bool

data Bool = False | True
data ''[]'' a = a : [a] | ''[]''

{- Library functions -}
data PackedString
foreign import ccall "primCStringToString" packedStringToString :: PackedString -> [Char]

error :: [Char] -> a
error s = undefined
undefined :: forall a . a
undefined = error "undefined"


(!!) :: [a] -> Int -> a
(x:xs) !! i = if i == 0
              then x
              else xs !! (i - 1)

map f []        = []
map f (x:xs)    = f x : (map f xs)

filter p []     = []
filter p (x:xs) = if p x
                  then x : (filter p xs)
                  else filter p xs

head (x:_) = x

iterate f x = x : iterate f (f x)
{- Primes code -}
suCC :: Int -> Int
suCC x = x + 1

isdivs :: Int  -> Int -> Bool
isdivs n x = (mod x n) /= 0

the_filter :: [Int] -> [Int]
the_filter (n:ns) = filter (isdivs n) ns

primes :: [Int]
primes = map head (iterate the_filter (iterate suCC 2))

arg = 500

main = primes !! arg
