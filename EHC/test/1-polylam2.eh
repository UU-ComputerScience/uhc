let  v  ::  (Int,Char)
     v  =   (  (\f -> (f 3, f 'x'))
               :: (a->a)->(Int,Char)
            )  (\x -> x)
in   v
