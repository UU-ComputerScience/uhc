let  r1 :: (a :: Int)
     r1 = (a = 3, b = 'x')
in
let
     r2 :: (a :: Int)
     r2 = (r1 | b := 'c')
in   r1#a

