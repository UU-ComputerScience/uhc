let  class A a where
       aa :: a -> Int
     class A a => B a where
       bb :: a -> Int
     instance dAInt <~ A Int where
       aa = \x -> x
     instance dBInt <~ B Int where
       bb = \x -> x
in
let  f  ::  (# B a #) -> a -> (Int,Int)
     f  =   \x -> (aa 3,aa x)
in
let  v  = f 3
in   v
