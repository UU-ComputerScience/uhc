let id :: forall a . a -> a
    id = \x -> x
    v = (id 3,id 'x')
    xy :: exists a . (a, a->Int)
    xy = (3,id)
    (x,y) = xy
 in v
