let  x@(a,b) = (2,3)
     f = \x y -> y
     g = \x y -> let h = \x z -> f z y
                     data A = A Int | B Int
                 in  case A 3 of
                       A x -> h y z
                       q@(B x) -> h x z
     z = 3
in
let  h = \x y -> f y x
in   h 3 4
