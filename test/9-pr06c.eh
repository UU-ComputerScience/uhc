-- higher order pred instance use + instance proof
let  data L a = N | C a (L a)
     class A a where
       aa :: a -> a -> a
     instance dAInt <: A Int where
       aa = \x _ -> x
     instance dAL <: A a => A (L a) where
       aa = \x _ -> x
in
let  f  ::  (A a,A a => A (L a)) => a -> L a -> L a
     f  =   \x y -> aa (C x N) y
in
let  v  =   f 3 (C 4 N)
in   v
