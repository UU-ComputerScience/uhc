let  ord                ::  Char -> Int
     id                 ::  forall a . a -> a
     f                  ::  Int -> exists a . (a,a->Int)
     f                  =   \x ->  case x of
                                     2 -> (x    ,id   )
                                     3 -> ('a'  ,ord  )
     ((xx,fx),(yy,fy))  =   (f 2, f 3)
     x                  =   fx yy
in   x
