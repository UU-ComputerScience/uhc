-- simple pred inference + multiple pred occurrence
let  class A a where
       aa :: a -> a
     c :: a -> a -> a
in
let  f  =   \x -> c (aa x) (aa x)
in   f
