let  g1  ::  ((exists a . (a,a->Int)) -> Int) -> Int
     id  ::  a -> a
     ci  ::  Char -> Int
     f   =   \h ->  let  x1  = h (3,id)
                         x2  = h ('x',ci)
                         y1  = g1 h
                    in   x1
in   f

