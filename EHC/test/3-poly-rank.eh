let  f   ::  (a -> a) -> (Int,Char)
     f   =   \i -> (i 3,i 'x')
     id  ::  a -> a
     v   =   f id
in   f
