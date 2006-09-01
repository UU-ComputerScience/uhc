let  foreign import jazy "primAddInt" add :: Int -> Int -> Int
     inc = \n -> add n 1
in
let  twice = \f x -> f (f x)
in
let  quad = \f -> twice twice f
in   quad quad inc 0
