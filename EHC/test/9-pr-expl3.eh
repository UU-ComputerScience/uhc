let  class A a where
       aa :: a -> Int
     class A a => B a where
       bb :: a -> Int
in
let  f  ::  ((# B a #) -> a -> Int) -> Int
     v  =   f aa
in   v
