let  g1  ::  ((a->a) -> (b->b)) -> Int
     g2  ::  ((a->a) -> (a->a)) -> Int
     f   =   \h ->  let  y1  = g1 h
                         y2  = g2 h
                    in   3
in   f

