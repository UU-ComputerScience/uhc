let  g1  ::  (exists a . (a,a->Int)) -> Int
     f   =   \h ->  let  (v,ff)  = h
                         x1 = ff v
                         y1  = g1 h
                    in   x1
in   f

