-- subclass pred check + instance (missing A,B,C)
let  class A a where
       aa :: a -> a
     class A a => B a where
       bb :: a -> a
     class B a => C a where
       cc :: a -> a
     c :: forall a . forall b . forall c . a -> b -> c
in
let  f  ::  C a => a -> a
     f  =   \x -> c (aa x) (cc x)
     v  =   f 3
in   v
