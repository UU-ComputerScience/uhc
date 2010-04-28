let  data Eq a b = Eq, a=c, b=c
in
let  f = \Eq -> Eq
     g = \Eq -> \Eq -> Eq
in   f Eq

