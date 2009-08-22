let  g1  ::  ((exists a . (a,a->Int)) -> Int) -> Int
     g2  ::  ((Int,Int->Int) -> Int) -> Int
     id  ::  a -> a
     ci  ::  Char -> Int
     f   =   \h  ->  let  x1  =  g1 h
                          x2  =  g2 h
                          h1  =  (3,id)
                          h2  =  ('x',ci)
                          y1  =  h h1
                          y2  =  h h2
                     in   3
in   3
