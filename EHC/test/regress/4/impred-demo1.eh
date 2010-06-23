let  g   ::  (forall a . a->a) -> Int
     id  =   \x ->  x
     f   =   \h ->  let  x1  = h 3
                         x2  = h 'x'
                         y   = g h
                    in   x1
in   f id
