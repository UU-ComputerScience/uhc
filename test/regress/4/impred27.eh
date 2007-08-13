let  g1  ::  ((forall a . a -> a) -> Int) -> Int
     g2  ::  ((forall a . (a,a) -> (a,a)) -> Int) -> Int
     f   =   \h  ->
             \z  ->  let  x1  =  g1 h
                          x2  =  g2 h
                          h1  =  h z
                          z1  =  z (3,4)
                          z2  =  z ('x','y')
                     in   3
in   3
