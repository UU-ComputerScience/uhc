-- simple pred check
let  class A a where
       aa :: a -> a
in
let  f  ::  A a => a -> a
     f  =   \x -> aa x
in   f
