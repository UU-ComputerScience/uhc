let  f   ::  (a -> a) -> (Int,Char)
     f   =   \i -> (i 3, i 'x')
     id  ::  a -> a
     id  =   \x -> x
in   f id
