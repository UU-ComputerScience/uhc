let  class A a where
       aa :: a -> a
     instance dA :: A Int where
       aa = \x -> x
in   let  instance dA <: A a
          instance dA <: A Int
     in   aa 3
