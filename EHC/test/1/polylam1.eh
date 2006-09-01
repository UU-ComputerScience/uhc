let  v  ::  (Int,Char)
     v  =   (  (\f -> (f 3, f 'x'))
               :: (Char->Char)->(Int,Char)
            )  (\x -> x)
in   v
