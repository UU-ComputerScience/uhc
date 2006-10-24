let  r1 :: (a :: Int, b :: Char)
     r1 = (a = 3, b = 'x')
     r2 :: (a :: Int, b :: Char)
     r2 = (r1 | b = 'c')
in   r1#a
