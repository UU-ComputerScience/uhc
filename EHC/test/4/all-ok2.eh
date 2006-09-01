let f :: forall a . a -> exists b . (b,b->Int)
    (x,y) = (f 2, f 3)
 in x
