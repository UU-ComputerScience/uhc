let  g1  ::  (forall a . a -> a) -> Int
     g2  ::  (Int -> Int) -> Int
     id  =   \x ->  x
     f   =   \h  ->  let  x1  =  g1 h
                          x2  =  g2 h
                     in   x2
in   f id
