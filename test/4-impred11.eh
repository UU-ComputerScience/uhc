let  g   ::  (forall a . a -> (forall b . b->b) -> a) -> Int
     f   =   \h -> \z
                ->  let  z1 = z 2
                         z2 = z 'x'
                         x1  = h 3 z
                         x2  = h 'x' z
                         y   = g h
                    in   y
in   3
