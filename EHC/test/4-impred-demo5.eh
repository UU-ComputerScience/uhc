let  g   ::  (forall a . a->a) -> Int
     id  =   \x ->  x
     f   =   \h ->  let  y   = g h
                         x1  = h 3
                         x2  = h 'x'
                    in   x1
in   f id
