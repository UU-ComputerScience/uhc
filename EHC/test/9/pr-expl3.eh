let  class A a where
       aa :: a -> Int
     class A a => B a where
       bb :: a -> Int
     
in
let  instance A Int where
       aa = \x -> 5
     instance B Int where
       bb = \x -> x
in
let  f  ::  (B a => a -> Int) -> Int
     f  =   \g -> g 4
     v  =   f aa
in   v
