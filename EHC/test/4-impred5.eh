let  g1  ::  (exists a . (a,a->Int)) -> Int
     id  ::  a -> a
     f   =   \h ->  let  (v,f)  = h
                         x1 = f v
                         y1  = g1 h
                    in   x1
in   f

