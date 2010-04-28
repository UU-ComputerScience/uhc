let data Bool = False | True
in
let foreign import jazy "primAddInt" add :: Int -> Int -> Int
in
let twice = \f x -> f (f x)
    f    = add 1
in twice f 1
