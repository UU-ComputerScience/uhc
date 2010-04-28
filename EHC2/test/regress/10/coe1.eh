let  class A a where
       aa :: a -> a
in
let  instance A Int where
       aa = \x -> x
in
let  f :: (x :: A a => a -> a) -> A a => a -> a
     f = \g x -> g # x x
in
let  d = (x = \y -> y)
in
let  v1 = f d 3
in
let  f2 :: (x :: a -> a) -> A a => a -> a
     f2 = \d -> f d
in   v1

