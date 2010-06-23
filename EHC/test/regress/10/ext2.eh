let  foreign import jazy "primAddInt" primAddInt :: Int -> Int -> Int
in
let  r  ::  (e :: Int, a :: Int, c :: Char)
     r  =   (a = 3, e = 4, c = 'x')
in
let
     v  =   (r|d = 3, b='z')
     v2 =   (r|b='z',d = 3)
     f  =   \r -> (r|d = 3, b='z')
in
let  w  =   f r
in   w
