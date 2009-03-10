{-
 - This is a EHC modified version of th primes benchmark of the nofib/imaginary
 - Original code by ???
 - Changes to be compiled by EHC: John van Schie
 -}
tak :: Int -> Int -> Int -> Int
tak x y z = if not(y < x) then z
       else tak (tak (x-1) y z)
		(tak (y-1) z x)
		(tak (z-1) x y)

main = <PRINT_INT> tak <ARG1> <ARG2> <ARG3>
