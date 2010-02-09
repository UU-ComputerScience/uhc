-- simple pred inference
let  class A a where
       aa :: a -> a
in
let  f = \x -> aa x
in   f
