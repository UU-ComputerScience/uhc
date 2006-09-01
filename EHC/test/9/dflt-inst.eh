let  class A a where
       a1 :: a -> a -> Int
       a2 :: a -> a -> Int
       a2 = \x y -> a1 y x
     instance dA <: A Int where
       a1 = \x y -> x
in   let  v = a1 3 4
     in   v

