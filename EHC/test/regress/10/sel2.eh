let  foreign import jazy "primAddInt" primAddInt :: Int -> Int -> Int
in
let  r  ::  (c :: Int, a :: Int, b :: Char)
     r  =   (a = 3, c = 4, b = 'x')
     f  =   \r -> r#c
in
let  w  =   f r
in   w
