-- pred inference + instance derivation
let  data L a = N | C a (L a)
     class A a where
       aa :: a -> a -> a
     instance dAInt <: A Int where
       aa = \x _ -> x
     instance dAL <: A a => A (L a) where
       aa = \x _ -> x
     c :: forall a . forall b . forall c . a -> b -> c
in
let  f  ::  L (L Int) -> L (L Int)
     f  =   \x -> aa x (C (C 3 N) N)
     v  =   f (C (C 5 N) N)
in   v
