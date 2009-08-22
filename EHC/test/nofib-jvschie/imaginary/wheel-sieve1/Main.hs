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

main =	<PRINT_INT> primes!!<ARG1>
