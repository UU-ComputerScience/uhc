let  class A a where
       aa :: a -> Int
     instance dAInt <: A Int where
       aa = \x -> x
in
let  f  ::  A a => a -> Int
     f  =   \x -> aa x
in
let  v  = f 3
in   v
