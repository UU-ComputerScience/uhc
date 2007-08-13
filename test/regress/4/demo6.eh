let  chr  ::  Int -> Char
     f    ::  (exists a . (a,a->Int)) -> exists a . (a,a->Int,a->Char)
     f    =   \(v,i) -> (v,i,\x-> chr (i x))
     x    =   (3,\x->x)
     y    =   f x
in   y
