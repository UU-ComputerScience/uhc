{-
 - This is a EHC modified version of th primes benchmark of the nofib/imaginary
 - Original code by (grabbed from LML dist)
 - Changes to be compiled by EHC: John van Schie
 -}
main = <PRINT_INT> nsoln <ARG1>

nsoln nq = length (gen nq)
 where
    safe :: Int -> Int -> [Int] -> Bool
    safe x d []    = True
    safe x d (q:l) = x /= q && x /= q+d && x /= q-d && safe x (d+1) l

    gen :: Int -> [[Int]]
    gen n | n==0 = [[]]
          | True = [ (q:b) | b <- gen (n-1), q <- [1..nq], safe q 1 b]
