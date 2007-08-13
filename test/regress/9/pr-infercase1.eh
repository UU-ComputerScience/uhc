let  data Bool = False | True
in
let  class A a where
       aa :: a -> Int
     instance dAInt <: A Int where
       aa = \x -> x
in
let  v  =   case True of
              True   ->  \x -> x
              False  ->  aa              
in   v
