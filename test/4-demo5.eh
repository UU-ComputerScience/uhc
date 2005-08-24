let  chr                ::  Char -> Int
     f                  ::  forall a . a -> exists b . (b,b->Int)
     ((xx,fx),(yy,fy))  =   (f 2, f 3)
     x                  =   fx yy
in   x
