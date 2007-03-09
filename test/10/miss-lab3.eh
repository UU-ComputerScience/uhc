let  r1 :: (a :: Int, b :: Char)
     r1 = (a = 3, b = 'x')
in
let
     r2 :: (b :: Char)
     r2 = (r1 | b := 'c')
in   r1#a

