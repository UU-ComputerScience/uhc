let  class A a where
       aa :: a -> a
in
let  f :: (x :: A a => a -> a) -> a
     d = (x = \y -> y)
in
let  v1 = f d
     f2 :: (x :: a -> a) -> a
     f2 = \d -> f d
in   v1

