let  g1  ::  ((Int,a) -> (Int,a)) -> Int
     g2  ::  ((a,Int) -> (a,Int)) -> Int
     f   =   \h ->  let  x1  = h (3,4)
                         x2  = h (3,'x')
                         y1  = g1 h
                         x3  = h (3,4)
                         x4  = h ('c',4)
                         y2  = g2 h
                    in   y2
in   f

