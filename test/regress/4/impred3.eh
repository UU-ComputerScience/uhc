let  g   ::  (forall a . a->a) -> Int
     id  =   \x ->  x
     f   =   \h ->  let  x1  = h 3
                         y   = g h
                         x2  = h 'x'
                    in   x2
in   f id
