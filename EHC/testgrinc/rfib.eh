let data Bool = False | True
in
let foreign import jazy "primSubInt" substract :: Int -> Int -> Int
    foreign import jazy "primAddInt" add :: Int -> Int -> Int
    foreign import jazy "primGtInt"  gt :: Int -> Int -> Bool
in
let nfib = \n -> case gt n 1 of 
                     True -> let prev1 = nfib (substract n 1)
                                 prev2 = nfib (substract n 2)
                             in add 1 (add prev1 prev2)
                     False -> 1
in nfib 32
