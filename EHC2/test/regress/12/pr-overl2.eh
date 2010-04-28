let  class A a where
       aa :: a -> Int
     instance dAInt1 <: A Int where
       aa = \x -> 1
     instance A Int where
       aa = \x -> 2
in
let  v  =   aa {! dAInt1 <: A Int !} 3
in   v
