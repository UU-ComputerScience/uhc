let  g   ::  (a->a) -> Int
     id  =   \x -> x
     f   =   \h -> let  y   = g h
                   in   y
in   f id
