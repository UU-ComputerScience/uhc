{-
 - This is a EHC modified version of th primes benchmark of the nofib/imaginary
 - Original code by ???
 - Changes to be compiled by EHC: John van Schie
 -}

foreign import ccall "primSubInt" (-)   :: Int -> Int -> Int
foreign import ccall "primLtInt"  (<)   :: Int -> Int -> Bool

data Bool = False | True

not :: Bool -> Bool
not x =
  if x then False else True

tak :: Int -> Int -> Int -> Int
tak x y z = if not(y < x) then z
       else tak (tak (x-1) y z)
		(tak (y-1) z x)
		(tak (z-1) x y)

xs = 33
ys = 17
zs = 8

main = tak xs ys zs
