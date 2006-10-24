let  f   ::  (forall a . a -> a) -> (Int,Char)
     f   =   \i -> (i 3, i 'x')
     id  ::  forall a . a -> a
     id  =   \x -> x
in   f id
