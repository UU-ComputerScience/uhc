let  g   ::  (forall a . a->a) -> Int
     id  =   \x ->  x
     f   =   \h ->  let  y   = g h
                    in   y
in   f id
