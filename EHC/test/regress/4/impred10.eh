let  g   ::  (forall a . a->a) -> Int
     id  =   \x ->  x
     f   =   \h ->  let  x1  = h 3
                         x2  = h 'x'
                         h'  = h
                         y   = g h'
                    in   y
in   f id
