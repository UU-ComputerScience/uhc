let  class A a where
       aa :: a -> Int
in
let  f5 = \x -> aa x
in   f5 3
