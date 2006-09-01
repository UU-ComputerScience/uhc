let data I = I (forall a . a -> a)
    id = \x -> x
 in let v1 = case I id of
               I i -> let (a,b) = (i 3, i 'x')
                      in  b
     in v1
