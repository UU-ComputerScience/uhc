let id = \x -> x
    id2 :: a -> a
    id2 = \x -> x
    v2 = (id2 3,id2 'x')
 in let v = (id 3,id 'x')
     in v
