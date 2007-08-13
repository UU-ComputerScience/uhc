let  add  ::  Int -> Int -> Int
     add  =   \x y -> x
     f    ::  (Int -> Int) -> Int
     f    =   \g -> g 1
     v1   =   add 2 3
     v2   =   f (add 4)
in   v2
