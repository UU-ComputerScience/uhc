-- higher order pred instance use
let  data L a = N | C a (L a)
     class A a where
       aa :: a -> a -> a
in
let  f  ::  (A a,A a => A (L a)) => a -> L a -> L a
     f  =   \x y -> aa (C x N) y
in   f
