let  id  ::  a -> a
     g   ::  (forall a . a -> (forall b . b->b) -> a) -> Int
     f   =   \h -> \z
                ->  let  z1 = z 2
                         z2 = z 'x'
                         x1  = h 3 z
                         x2  = h 'x' z
                         y   = g h
                    in   y
     p   ::  forall a . a -> (forall b . b->b) -> a
     p   =   \a -> \i -> i a
     v1  =   f p id
     v2  =   f (\a -> \i -> i a) (\x ->x)
in   v2
