{-
 - This is a EHC modified version of th primes benchmark of the nofib/imaginary
 - Original code by ???
 - Changes to be compiled by EHC: John van Schie
 -}
suCC :: Int -> Int
suCC x = x + 1

isdivs :: Int  -> Int -> Bool
isdivs n x = (mod x n) /= 0

the_filter :: [Int] -> [Int]
the_filter (n:ns) = filter (isdivs n) ns

primes :: [Int]
primes = map head (iterate the_filter (iterate suCC 2))

arg = <ARG1>

main = <PRINT_INT> primes !! arg
