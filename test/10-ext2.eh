let  r  ::  (e :: Int, a :: Int, c :: Char)
     r  =   (a = 3, e = 4, c = 'x')
     v  =   (r|d = 3, b='z')
     v2 =   (r|b='z',d = 3)
     f  =   \r -> (r|d = 3, b='z')
in
let  w  =   f r
in   w
