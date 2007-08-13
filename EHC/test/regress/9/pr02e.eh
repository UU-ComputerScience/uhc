-- subclass pred check + instance
let  class A a where
       aa :: a -> a
     class A a => B a where
       bb :: a -> a
     class B a => C a where
       cc :: a -> a
     instance dAInt <: A Int where
       aa = \x -> x
     instance dBInt <: B Int where
       bb = \x -> x
     instance dCInt <: C Int where
       cc = \x -> x
     c :: a -> a -> a
     c = \x _ -> x
in
let  f  ::  C a => a -> a
     f  =   \x -> c (aa x) (cc x)
     v  =   f 3
in   v
