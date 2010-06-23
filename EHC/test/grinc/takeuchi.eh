let data Bool = False | True
in
let foreign import jazy "primSubInt" substract :: Int -> Int -> Int
    foreign import jazy "primGtInt"  gt :: Int -> Int -> Bool
in
let tak = \x y z -> case gt x y of
                     True -> let a1 = tak (substract x 1) y z
                                 a2 = tak (substract y 1) z x
                                 a3 = tak (substract z 1) x y
                             in tak a1 a2 a3
                     False -> z
in tak 18 12 6
