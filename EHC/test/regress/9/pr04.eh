-- pred inference + instance derivation
let  data L a = N | C a (L a)
     class A a where
       aa :: a -> a -> a
     instance dAInt <: A Int where
       aa = ...
     instance dAL <: A a => A (L a) where
       aa = ...
     c :: forall a . forall b . forall c . a -> b -> c
in
let  f  =   \x -> aa x (C (C 3 N) N)
in   f
