-- subclass pred check + instance (missing A,B)
let  class A a where
       aa :: a -> a
     class A a => B a where
       bb :: a -> a
     class B a => C a where
       cc :: a -> a
     instance dCInt <: C Int where
       cc = \x -> x
     c :: a -> a -> a
     c = \x y -> x
in
let  f  ::  C a => a -> a
     f  =   \x -> c (aa x) (cc x)
     v  =   f 3
in   v
