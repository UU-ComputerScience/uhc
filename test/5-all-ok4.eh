let data I = I (a -> a)
    id = \x -> x
 in let v1 = case I id of
               I i -> (i 3, i 'x')
     in v1
