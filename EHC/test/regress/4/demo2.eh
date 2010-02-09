let  id  ::  a -> a
     id  =   \x -> x
     f   ::  (forall a . a -> a) -> ...
     f   =   \i -> (i 3,i 'x')
in   f id
